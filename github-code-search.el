;;; github-code-search.el --- Configure code search -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/github-code-search
;; Keywords: tools
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (transient "0.4.1") (ghub "3.6.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file configures operations with code search

;; Commands

;; M-x `github-code-search'
;;      Search code on github for a given language and filename.

;; Customization

;; `github-code-search-word-browse-fn'
;;      Function to browse results of code search.

;;; Code:

(require 'transient)

(declare-function json-read-from-string "json")

(defvar github-code-search-langs-alist nil)

(require 'ghub)

(defcustom github-code-search-word-browse-fn 'browse-url
  "Function to browse results of code search."
  :type 'function
  :group 'github-code-search)

(defcustom github-code-search-user ""
  "Function to browse results of code search."
  :type 'string
  :group 'github-code-search)

(defcustom github-code-search-token 'forge
  "Function to browse results of code search."
  :type '(radio (string :tag "Github Token")
                (symbol :tag "Auth source marker" forge)
                (function :tag "Function that returns the Github Token"))
  :group 'github-code-search)
(defvar github-code-search-langs nil)

(defun github-code-search-get-auth-source-users ()
  "Return list of users in auth sources with host `api.github.com'."
  (let ((all-users (delq nil (mapcar (lambda (pl)
                                       (plist-get pl :user))
                                     (auth-source-search
                                      :host "api.github.com"
                                      :require
                                      '(:user :secret)
                                      :max
                                      most-positive-fixnum))))
        (suffix (regexp-quote (concat "^"
                                      (symbol-name github-code-search-token)))))
    (or (seq-filter (apply-partially #'string-match-p suffix) all-users)
        all-users)))

;;;###autoload
(defun github-code-search-change-user (&optional prompt initial-input history)
  "Change the user for authentication prompting with string PROMPT.

Optional argument PROMPT is the initial value to be displayed in the prompt.

If non-nil, optional argument INITIAL-INPUT is a string to insert before
reading.

The third arg HISTORY, if non-nil, specifies a history list and optionally the
initial position in the list."
  (interactive)
  (let ((user
         (if (stringp github-code-search-token)
             (read-string (or prompt "User: ") initial-input history)
           (let* ((alist (mapcar (lambda (it)
                                   (let ((parts (split-string it "[\\^]" t)))
                                     (cons (pop parts)
                                           (pop parts))))
                                 (github-code-search-get-auth-source-users)))
                  (annotf (lambda (str)
                            (format "^%s" (cdr (assoc str alist)))))
                  (login-name (completing-read (or prompt
                                                   "Github user name: ")
                                               (lambda (str pred action)
                                                 (if (eq action 'metadata)
                                                     `(metadata
                                                       (annotation-function
                                                        . ,annotf))
                                                   (complete-with-action
                                                    action alist
                                                    str
                                                    pred)))
                                               nil
                                               nil
                                               initial-input
                                               history))
                  (marker (cdr (assoc login-name alist))))
             (when-let ((marker (and marker (intern marker))))
               (unless (eq marker github-code-search-token)
                 (setq github-code-search-token marker)))
             login-name))))
    (if (string-empty-p user)
        nil
      user)))

(defun github-code-search-word-or-region ()
  "Get current word or region."
  (or (when (and (region-active-p)
                 (use-region-p))
        (string-trim (buffer-substring-no-properties
                      (region-beginning)
                      (region-end))))
      (when-let ((symb (symbol-at-point)))
        (symbol-name symb))))

(defun github-code-search-query-from-alist (query-alist &optional keep-empty
                                                        semicolons)
  "Build a query-string from QUERY-ALIST.
If KEEP-EMPTY is non nil, include queries without values, else remove them.
When SEMICOLONS is given, the separator will be \";\"."
  (let ((filtered-list (delete
                        nil
                        (seq-filter
                         (lambda
                           (&rest args)
                           (funcall
                            (apply-partially #'<= (if keep-empty 1 2))
                            (apply #'length args)))
                         (mapcar (apply-partially #'delete nil)
                                 query-alist)))))
    (url-build-query-string
     filtered-list semicolons keep-empty)))

(defun github-code-search-get-default-language ()
  "Return default github language depending on `major-mode'."
  (pcase major-mode
    ((or 'emacs-lisp-mode 'lisp-interaction-mode)
     "elisp")
    ((or 'python-ts-mode 'python-mode)
     "python")
    ((or 'cmake-ts-mode 'cmake-mode)
     "cmake")
    ((or 'c-or-c++-ts-mode
         'c-ts-base-mode
         'c-mode)
     "c")
    ((or 'yaml-ts-mode 'yaml-mode)
     "yaml")
    ((or 'html-ts-mode 'html-mode)
     "html")
    ((or 'css-ts-mode 'css-ts-mode)
     "css")
    ((or 'typescript-ts-mode
         'typescript-ts-base-mode
         'typescript-mode)
     "typescript")
    ((or 'bash-ts-mode 'sh-mode)
     "shell")
    ((or 'java-ts-mode)
     "java")
    ((or 'js-ts-mode
         'js-mode 'js-base-mode 'js2-mode)
     "javascript")
    ((or 'json-ts-mode 'json-mode)
     "json")
    ((or 'clojurescript-mode 'clojure-mode)
     "clojure")
    ('org-mode
     "org")))

(defun github-code-search-read-filename (&optional prompt initial-input history)
  "Read filename with PROMPT, INITIAL-INPUT and HISTORY for github code search."
  (let ((filename (read-string
                   (or prompt "File: ")
                   (or initial-input
                       (when buffer-file-name
                         (file-name-nondirectory buffer-file-name)))
                   history)))
    (when (and filename
               (not (string-empty-p filename)))
      filename)))

(defun github-code-search-get-query-alist (code language filename &rest params)
  "Create a query string for GitHub CODE search.

Argument PARAMS is a variable number of additional parameters that can be passed
to the function.
Argument FILENAME is a string that represents the name of a file.
Argument LANGUAGE is a string that represents the programming LANGUAGE.
Argument CODE is a string that represents the CODE to search for."
  (let* ((type
          (cond ((and filename (or (not code)
                                   (string-empty-p code)))
                 "path")
                (t "Code")))
         (q (string-join
             (append
              (list (if filename
                        (concat "path:" filename " " code)
                      code))
              (when language
                (list (concat " language:" language)))
              (remove nil params))
             "")))
    `(("q" ,q)
      ("type" ,type))))

(defun github-code-search-make-query (code language filename &rest params)
  "Search CODE on github for a given LANGUAGE and FILENAME.
PARAMS is added to q."
  (let* ((type (cond ((and filename (or (not code)
                                        (string-empty-p code)))
                      "path")
                     (t "Code")))
         (q (string-join
             (append
              (list (if filename
                        (concat "path:" filename " " code)
                      code))
              (when language
                (list (concat " language:" language)))
              (remove nil params))
             "")))
    (github-code-search-query-from-alist
     `(("q" ,q)
       ("type" ,type)))))

(defun github-code-search-json-parser (str)
  "Parse STR with natively compiled function or with json library.
If Emacs has libjansson support, parse it with natively compiled function,
otherwise use the parsing routines from the json library."
  (if (and (fboundp 'json-parse-string)
           (fboundp 'json-available-p)
           (json-available-p))
      (json-parse-string str)
    (require 'json)
    (json-read-from-string str)))

(defun github-code-search-init-languages ()
  "Fetch github languages and stotre them to `github-code-search-langs-alist'."
  (setq github-code-search-langs-alist
        (mapcar
         (lambda (it)
           (let-alist it
             (cons .name .aliases)))
         (json-parse-string
          (github-code-search-download-url
           "https://api.github.com/languages")
          :object-type 'alist
          :array-type 'list))))

(defun github-code-search-download-url (url)
  "Download URL and return string."
  (let ((download-buffer (url-retrieve-synchronously url)))
    (prog1
        (with-current-buffer download-buffer
          (set-buffer download-buffer)
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (forward-char)
          (delete-region (point-min)
                         (point))
          (buffer-string))
      (kill-buffer download-buffer))))


;;;###autoload
(defun github-code-search-browse ()
  "Search from transient."
  (interactive)
  (let* ((args (transient-args transient-current-command))
         (filename (transient-arg-value "--path=" args))
         (code (transient-arg-value "--code=" args))
         (language (transient-arg-value "--language="
                                        args))
         (user (transient-arg-value "--user=" args))
         (not-user (transient-arg-value "--not-user=" args)))
    (funcall github-code-search-word-browse-fn
             (concat "https://github.com/search?"
                     (github-code-search-make-query
                      code language filename
                      (when user (concat " user:" user))
                      (when not-user (concat " -user:" not-user)))))))

(defun github-code-search-annotate-language (language)
  "Annotate github LANGUAGE with it's aliases."
  (let ((item
         (cdr-safe (assoc language github-code-search-langs-alist))))
    (concat " " (or (if (stringp item)
                        item
                      (if (listp item)
                          (string-join item " ")
                        (car item)))
                    ""))))

(defun github-code-search-render-text-match (item)
  "Highlight text matches in a GitHub code search result.

Argument ITEM is an alist containing the fragment and matches for a text match
in a GitHub code search."
  (let ((frag (alist-get 'fragment item))
        (matches (alist-get 'matches item)))
    (dolist (alist matches)
      (let-alist alist
        .indices
        (add-text-properties (car .indices)
                             (cadr .indices) '(face highlight)
                             frag)))
    frag))

(defun github-code-search-auth ()
  "Prompt user to enter GitHub username for code search."
  (while (or (not github-code-search-user)
             (string-empty-p github-code-search-user))
    (setq github-code-search-user (github-code-search-change-user "User: "))))



(defun github-code-search-popup (content &optional buffer fn &rest args)
  "Display GitHub code search CONTENT in a popup window.

Argument BUFFER is an optional argument that specifies the buffer in which the
code search results will be displayed.
If not provided, a new buffer named \"`*github-code-search*'\" will be created.
Argument FN is extra function to apply with ARGS."
  (with-current-buffer (or (and buffer
                                (get-buffer-create buffer))
                           (get-buffer-create "*github-code-search*"))
    (with-current-buffer-window
        buffer
        (cons (or 'display-buffer-in-direction)
              '((window-height . window-preserve-size)))
        (lambda (window _value)
          (with-selected-window window
            (setq buffer-read-only t)
            (erase-buffer)
            (let ((inhibit-read-only t))
              (when content
                (save-excursion
                  (insert content)))
              (when fn
                (apply fn args))))))))

(defun github-code-search-request (code query)
  "Search for CODE on GitHub.

Argument QUERY is a string that represents the search query for the CODE search."
  (github-code-search-auth)
  (let* ((url (concat (format "search/code?q=%s" code)
                      (or query "")
                      "&per_page=100"))
         (response (ghub-request "GET"
                                 url
                                 nil
                                 :forge 'github
                                 :headers
                                 `(("Accept" . "application/vnd.github.text-match+json"))
                                 :auth github-code-search-token
                                 :username github-code-search-user
                                 :host "api.github.com"))
         (items (alist-get 'items response)))
    items))

(defun github-code-search-fontify (code name)
  "Highlight CODE syntax for GitHub CODE search.

Argument NAME is the name of the file being searched for in the GitHub CODE
search.
Argument CODE is the CODE snippet that will be inserted into a temporary buffer
for fontification."
  
  (with-temp-buffer
    (insert code)
    (let ((buffer-file-name (expand-file-name name
                                              default-directory)))
      (ignore-errors
        (set-auto-mode)
        (font-lock-ensure)))
    (buffer-string)))

(defun github-code-search-download-code (item &optional search-str)
  "Download and display GitHub code based on a provided ITEM and search string.

Argument ITEM is an association list that contains information about a specific
item from a GitHub code search.
Argument SEARCH-STR is an optional string that specifies the text to search for
within the downloaded code."
  (let* ((data (json-parse-string
                (github-code-search-download-url (alist-get 'url item))
                :object-type 'alist
                :array-type 'list))
         (github-file (or (alist-get 'name item)
                          (alist-get 'path item)))
         (file (expand-file-name github-file
                                 default-directory))
         (code (base64-decode-string (alist-get 'content data))))
    (github-code-search-popup
     code
     (get-buffer-create (concat "*" github-file
                                "*"))
     (lambda ()
       (let ((buffer-file-name file))
         (ignore-errors
           (set-auto-mode)
           (font-lock-ensure)
           (github-code-search-higlight-matches search-str)
           (goto-char (point-min))
           (when search-str
             (re-search-forward (regexp-quote search-str) nil t 1))
           (setq header-line-format github-file)))))))

(defun github-code-search-higlight-matches (str)
  "Highlight matches of a given string in GitHub code search.

Argument STR is a string that represents the pattern to be highlighted in the
code."
  (let ((case-fold-search t))
    (highlight-regexp (regexp-quote str))))
(defvar org-src-lang-modes)

(defun github-code-search-lang-to-org-lang (lang)
  "Convert a GitHub code search language to an `org-mode' language.

Argument LANG is the programming language for which the function will determine
the corresponding language in Org mode."
  (require 'org)
  (let ((babel-langs (append (mapcar (lambda (it)
                                       (symbol-name (car (reverse it))))
                                     (cdr (nth 1
                                               (memq :key-type
                                                     (get
                                                      'org-babel-load-languages
                                                      'custom-type)))))
                             (mapcar #'car org-src-lang-modes)
                             (mapcar (lambda (i)
                                       (symbol-name (cdr i)))
                                     org-src-lang-modes))))
    (if (or (assoc lang org-src-lang-modes)
            (member lang babel-langs))
        lang
      (let ((variants
             (seq-find (lambda (langs)
                         (member lang langs))
                       github-code-search-langs-alist)))
        (seq-find (lambda (it)
                    (member it babel-langs))
                  variants)))))

(defvar github-code-search-src-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")
                'github-code-search-load-code)
    (define-key map (kbd "C-c C-o")
                'github-code-search-load-code)
    (define-key map (kbd "C-<return>")
                'github-code-search-load-code)
    map))



;;;###autoload
(defun github-code-search-code ()
  "Search from transient."
  (interactive)
  (let* ((args (transient-args transient-current-command))
         (code (transient-arg-value "--code=" args))
         (result (seq-reduce
                  (lambda (acc arg)
                    (let ((value
                           (transient-arg-value arg args)))
                      (if (not value)
                          acc
                        (let* ((neg (string-prefix-p "--not-" arg))
                               (query (if neg
                                          (replace-regexp-in-string
                                           "^--not-\\|=$" ""
                                           arg)
                                        (replace-regexp-in-string "^--\\|=$" ""
                                                                  arg)))
                               (separator (if neg "-" "+")))
                          (setq acc (concat acc separator query ":" value))))))
                  '("--path="
                    "--language="
                    "--not-user=")
                  ""))
         (items (github-code-search-request code result)))
    (github-code-search-popup nil
                              (get-buffer-create "*github-code-search*")
                              #'github-code-search-insert-matches
                              items code)
    (select-window (get-buffer-window "*github-code-search*"))))

(defun github-code-search-insert-matches (items code)
  "Insert matches from a GitHub CODE search into the current buffer.

Argument ITEMS is a list of items that will be iterated over in the
function.

Argument CODE is the specific CODE that will be inserted for each item
in the items list and highlighted in the search results."
  (dolist (item items)
    (github-code-search-insert-item item code))
  (github-code-search-higlight-matches code)
  (goto-char (point-min)))

(defun github-code-search-load-code ()
  "Download and load code from GitHub based on a search item."
  (interactive)
  (let ((item (get-text-property (point) 'github-code-search-item)))
    (github-code-search-download-code item
                                      (get-text-property (point)
                                                         'github-code-search-str))))


(defun github-code-search-insert-item (item search-str)
  "Insert a GitHub code search ITEM into the current buffer.

Argument ITEM is an alist containing the details of a specific item from a
GitHub code search.
Argument SEARCH-STR is the string that was used to perform the GitHub code
search."
  (let* ((name (alist-get 'name item))
         (path (alist-get 'path item))
         (html_url (alist-get 'html_url item))
         (text-matches (alist-get 'text_matches item))
         (code (mapconcat #'github-code-search-render-text-match
                          text-matches
                          "\n\n"))
         (title (or path name)))
    (when (or name path)
      (setq code
            (with-temp-buffer
              (insert code)
              (let
                  ((buffer-file-name (expand-file-name (or name path)
                                                       default-directory)))
                (ignore-errors
                  (set-auto-mode)
                  (font-lock-ensure)))
              (buffer-string))))
    (insert "\n" (buttonize title #'browse-url html_url) "\n")
    (let ((beg (point))
          (end))
      (insert
       code)
      (setq end (point))
      (add-text-properties beg end
                           `(html_url ,html_url
                                      github-code-search-item ,item
                                      github-code-search-str ,search-str
                                      keymap ,github-code-search-src-keymap))
      (insert  "\n"))))



;;;###autoload (autoload 'github-code-search "github-code-search" nil t)
(transient-define-prefix github-code-search ()
  "Command dispatcher."
  :incompatible '(("--path=" "--language="))
  :value
  (lambda ()
    (let* ((code (github-code-search-word-or-region))
           (code-arg
            (when (and code (not (string-empty-p code)))
              (concat "--code=" code))))
      (delete nil
              (list
               code-arg
               (if-let ((value (github-code-search-get-default-language))
                        (l "--language="))
                   (concat l value)
                 (if-let ((ext
                           (when buffer-file-name
                             (file-name-extension buffer-file-name))))
                     (concat "--path=" "*." ext)
                   (when (and buffer-file-name (not code-arg))
                     (concat "--path="
                             (file-name-nondirectory buffer-file-name)))))))))
  ["Arguments"
   ("c" "code" "--code="
    :class transient-option
    :always-read t)
   ("l" "language" "--language="
    :choices github-code-search-langs)
   ("p" "path" "--path=" :class transient-option)
   ("u" "user" "--user=" :class transient-option)]
  ["Exclude"
   ("U" "not-user" "--not-user=" :class transient-option)]
  ["Actions"
   ("C-c C-o" "Browse" github-code-search-browse)
   ("RET" "Run" github-code-search-code)]
  (interactive)
  (setq github-code-search-langs-alist (or github-code-search-langs-alist
                                           (github-code-search-init-languages)))
  (setq github-code-search-langs
        (or github-code-search-langs
            (mapcan #'cdr (copy-tree
                           github-code-search-langs-alist))))
  (transient-setup #'github-code-search))

(provide 'github-code-search)
;;; github-code-search.el ends here