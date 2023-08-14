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

;; Search code on GitHub within Emacs.

;; Main Commands

;; M-x `github-code-search' This will open a menu for GitHub code search where
;; you can enter a specific code query. After entering the search term, you can
;; either press `RET' to load and display the results in Emacs Mode, or press
;; `C-c C-o' to open the results in the browser.

;; While displaying the results in Emacs, you can use the following commands:

;; | Key   | Command                                      |
;; |-------+----------------------------------------------|
;; | `RET' | Show full code result                        |
;; | `.'   | Toggle the exactness of a GitHub code search |
;; | `e'   | Toggle the exactness of a GitHub code search |
;; | `u'   | Toggle showing only unique results           |
;; | `+'   | Load next page                               |
;; | `g'   | Revert buffer                                |

;; You can edit keybinding by modifiyng `github-code-search-results-mode-map'.


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
  "A string containing GitHub username."
  :type 'string
  :group 'github-code-search)

(defcustom github-code-search-token 'github-code-search
  "GitHub OAuth token or suffix added to the USERNAME^MARKER in auth sources.

This variable can either hold an explicit OAuth token as a string,
in which case `github-code-search' will use this token for authentication, or a
symbol that is used as a suffix for the `login' field in `auth-sources'.

For example, if you set this to the symbol `github-code-search', which is
the default setting,you would add an entry like this to your auth-sources:

\"machine api.github.com login GITHUB_USERNAME^github-code-search password
GITHUB_TOKEN\"."
  :type '(radio (string :tag "Github Token")
                (symbol :tag "Auth source marker" github-code-search)
                (function :tag "Function that returns the Github Token"))
  :group 'github-code-search)

(defcustom github-code-search-per-page-limit 100
  "The number of results per page. It should be the value between 30 to 100."
  :type 'integer
  :group 'github-code-sarch)

(defvar github-code-search-langs nil)
(defvar github-code-search-result-buffer-name "*github-code-search*")

(defvar-local github-code-search--search-code nil)
(defvar-local github-code-search--search-extra-query nil)
(defvar-local github-code-search-response nil)
(defvar-local github-code-search-exact nil)
(defvar-local github-code-search-total-count nil)
(defvar-local github-code-search-page 1)
(defvar-local github-code-search-uniq nil)

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

(defun github-code-search-json-parse-string (str &optional object-type
                                                 array-type null-object false-object)
  "Parse STR with natively compiled function or with json library.

The argument OBJECT-TYPE specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `alist'.

The argument ARRAY-TYPE specifies which Lisp type is used
to represent arrays; `array'/`vector' and `list'.

The argument NULL-OBJECT specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The argument FALSE-OBJECT specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
  (if (and (fboundp 'json-parse-string)
           (fboundp 'json-available-p)
           (json-available-p))
      (json-parse-string str
                         :object-type (or object-type 'alist)
                         :array-type
                         (pcase array-type
                           ('list 'list)
                           ('vector 'array)
                           (_ 'array))
                         :null-object (or null-object :null)
                         :false-object (or false-object :false))
    (require 'json)
    (let ((json-object-type (or object-type 'alist))
          (json-array-type
           (pcase array-type
             ('list 'list)
             ('array 'vector)
             (_ 'vector)))
          (json-null (or null-object :null))
          (json-false (or false-object :false)))
      (json-read-from-string str))))

(defun github-code-search-init-languages ()
  "Fetch github languages and stotre them to `github-code-search-langs-alist'."
  (setq github-code-search-langs-alist
        (mapcar
         (lambda (it)
           (let-alist it
             (cons .name .aliases)))
         (github-code-search-json-parse-string
          (github-code-search-download-url
           "https://api.github.com/languages")
          'alist
          'list))))

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
  "Open results in browser."
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
                           (get-buffer-create
                            github-code-search-result-buffer-name))
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


(defun github-code-search-async-request (code query page callback)
  "Send an asynchronous request to GitHub's CODE search API.

Argument CODE is the code segment or keyword that the user wants to
search for in the GitHub codebase.
Argument QUERY is an optional additional search term that can be used to
refine the search results.
Argument PAGE is the specific PAGE number of the search results that the
user wants to view.
Argument CALLBACK is a function that will be called once the search
results are returned, with the search results passed as an argument."
  (github-code-search-auth)
  (ghub-request "GET" (format "search/code?q=%s&per_page=100&page=%s"
                              (concat
                               code
                               (or
                                query
                                ""))
                              page)
                nil
                :auth github-code-search-token
                :username github-code-search-user
                :headers
                `(("Accept" . "application/vnd.github.text-match+json"))
                :forge 'github
                :host "api.github.com"
                :callback
                (lambda (value _headers _status _req)
                  (let ((buffer (get-buffer
                                 github-code-search-result-buffer-name)))
                    (when (and
                           (buffer-live-p
                            buffer)
                           (equal code
                                  (buffer-local-value
                                   'github-code-search--search-code
                                   buffer))
                           (equal query
                                  (buffer-local-value
                                   'github-code-search--search-extra-query
                                   buffer)))
                      (when callback
                        (funcall callback value)))))))

(defun github-code-search-next-page (&rest _)
  "Increment the page number and request the next page of GitHub code search."
  (interactive)
  (setq github-code-search-page (1+ github-code-search-page))
  (github-code-search-request github-code-search--search-code
                              github-code-search--search-extra-query
                              github-code-search-exact
                              github-code-search-uniq))

(defun github-code-search-count-rendered-items ()
  "Remove the footer from the GitHub code search results."
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (text-property-search-forward
              'github-code-search-item)
        (setq count (1+ count)))
      count)))

(defun github-code-search-delete-footer ()
  "Remove the footer from the GitHub code search results."
  (save-excursion
    (when-let ((prop-obj (save-excursion
                           (goto-char (point-min))
                           (text-property-search-forward
                            'github-code-search-pagination-button t
                            'equal))))
      (let ((beg (prop-match-beginning prop-obj))
            (end (prop-match-end prop-obj)))
        (when (and beg end)
          (delete-region beg end))))))

(defun github-code-search-render-footer ()
  "Render the footer for the GitHub code search page."
  (github-code-search-delete-footer)
  (save-excursion
    (goto-char (point-max))
    (newline)
    (insert
     (if
         (and github-code-search-total-count
              (> github-code-search-total-count (length
                                                 github-code-search-response)))
         (buttonize
          (format "[Next page %s]" (1+ github-code-search-page))
          #'github-code-search-next-page)
       (buttonize
        "[Next page]"
        #'message "No next page")))
    (add-text-properties (line-beginning-position)
                         (line-end-position)
                         '(github-code-search-pagination-button t))))
(defun github-code-search-filter-uniq (items)
  "Remove duplicate ITEMS from a list based on specific criteria.

Argument ITEMS is a list of items that will be processed by the function to
remove duplicates."
  (seq-uniq
   items
   (lambda (a b)
     (apply #'equal
            (mapcar
             (lambda (item)
               (mapconcat (apply-partially
                           #'alist-get
                           'fragment)
                          (alist-get
                           'text_matches
                           item)
                          "\n"))
             (list a b))))))

(defun github-code-search-filter-exact (items)
  "Filter GitHub code search results for exact matches.

Argument ITEMS is a sequence of items that will be filtered based on whether the
text matches a specific regular expression."
  (let ((re (regexp-opt
             (list github-code-search--search-code) 'symbols)))
    (seq-filter
     (lambda (item)
       (let ((text (mapconcat (apply-partially
                               #'alist-get
                               'fragment)
                              (alist-get
                               'text_matches
                               item)
                              "\n")))
         (string-match-p
          re
          text)))
     items)))

(defun github-code-search-revert ()
  "Revert the GitHub code search buffer and update it with new search results."
  (let ((filtered github-code-search-response)
        (label))
    (when github-code-search-uniq
      (setq filtered (github-code-search-filter-uniq filtered)))
    (when github-code-search-exact
      (setq filtered (github-code-search-filter-exact filtered)))
    (let ((inhibit-read-only t)
          (pos (point)))
      (erase-buffer)
      (github-code-search-insert-matches
       filtered
       github-code-search--search-code)
      (github-code-search-render-footer)
      (setq label (mapconcat #'car
                             (seq-filter #'cdr
                                         (list
                                          (cons "[Uniq]"
                                                github-code-search-uniq)
                                          (cons "[Exact]"
                                                github-code-search-exact)))
                             " "))
      (github-code-search--update-header-line
       (if (string-empty-p (string-trim label))
           (format (concat " Page %s (%s/%s) ")
                   github-code-search-page
                   (length github-code-search-response)
                   github-code-search-total-count)
         (format " Page %s (%s/%s) - %s (%s)"
                 github-code-search-page
                 (length github-code-search-response)
                 github-code-search-total-count
                 label
                 (length filtered)))
       'warning)
      (ignore-errors (goto-char pos)))))

(defun github-code-search-toggle-exact ()
  "Toggle the exactness of a GitHub code search."
  (interactive)
  (setq github-code-search-exact (not github-code-search-exact))
  (github-code-search-revert))

(defun github-code-search-toggle-uniq ()
  "Toggle the uniqueness of GitHub code search results."
  (interactive)
  (setq github-code-search-uniq (not github-code-search-uniq))
  (github-code-search-revert))

(defun github-code-search-request (code query &optional exact uniq)
  "Create a GitHub CODE search request and display the results in a buffer.

Argument CODE is the code snippet or keyword that the user wants to search for
in the GitHub codebase.
Argument QUERY is the additional search parameters or filters that the user
wants to apply to the code search.
Argument EXACT is an optional argument that, when true, will make the search
return only EXACT matches to the provided code.
Argument UNIQ is an optional argument that, when true, will make the search
return only unique matches, removing any duplicates from the results."
  (let ((buffer (get-buffer-create
                 github-code-search-result-buffer-name)))
    (with-current-buffer buffer
      (when (or (not (equal code github-code-search--search-code))
                (not (equal query github-code-search--search-extra-query)))
        (let ((inhibit-read-only t))
          (erase-buffer))
        (setq github-code-search-page 1)
        (setq github-code-search--search-code code)
        (setq github-code-search-response nil)
        (setq github-code-search-total-count nil)
        (setq github-code-search-exact exact)
        (setq github-code-search-uniq uniq)
        (setq github-code-search--search-extra-query query))
      (unless (symbol-value 'github-code-search-results-mode)
        (github-code-search-results-mode 1))
      (github-code-search--update-header-line " Loading..." 'warning)
      (setq github-code-search--search-code code)
      (setq github-code-search--search-extra-query query)
      (github-code-search-async-request
       code query github-code-search-page
       (lambda (value)
         (let* ((items (alist-get 'items value))
                (filtered-items items))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (setq github-code-search-exact exact)
               (setq github-code-search-uniq uniq)
               (when github-code-search-uniq
                 (setq filtered-items (github-code-search-filter-uniq filtered-items)))
               (when github-code-search-exact
                 (setq filtered-items (github-code-search-filter-exact filtered-items)))
               (setq github-code-search-total-count (or
                                                     (alist-get 'total_count value)
                                                     github-code-search-total-count))
               (setq buffer-read-only t)
               (let ((inhibit-read-only t))
                 (when items
                   (setq github-code-search-response
                         (if github-code-search-response
                             (append github-code-search-response items)
                           items)))
                 (when filtered-items
                   (save-excursion
                     (goto-char (point-max))
                     (github-code-search-insert-matches
                      filtered-items
                      code)))
                 (github-code-search-render-footer)
                 (github-code-search--update-header-line
                  (format " Page %s (%s/%s)"
                          github-code-search-page
                          (length github-code-search-response)
                          github-code-search-total-count)
                  'warning)))
             (unless (get-buffer-window buffer)
               (pop-to-buffer buffer)))))))))

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
  (let ((case-fold-search t)
        (re (regexp-quote str)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re nil t 1)
        (add-face-text-property
         (match-beginning 0)
         (match-end 0)
         'highlight)))))

(defvar github-code-search-src-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")
                #'github-code-search-load-code-result-at-point)
    (define-key map (kbd "C-c C-o")
                #'github-code-search-load-code-result-at-point)
    (define-key map (kbd "C-<return>")
                #'github-code-search-load-code-result-at-point)
    map))


;;;###autoload
(defun github-code-search-code ()
  "Search from transient."
  (interactive)
  (let* ((args (transient-args transient-current-command))
         (code (transient-arg-value "--code=" args))
         (exact (transient-arg-value "--exact" args))
         (uniq (transient-arg-value "--uniq" args))
         (result (seq-reduce
                  (lambda (acc arg)
                    (let ((value
                           (transient-arg-value arg args)))
                      (if (not value)
                          acc
                        (let* ((neg (string-prefix-p "--not-" arg))
                               (query (if neg
                                          (replace-regexp-in-string
                                           "\\(^--not-\\)\\|\\(=$\\)" ""
                                           arg)
                                        (replace-regexp-in-string "^--\\|=$" ""
                                                                  arg)))
                               (separator (if neg "+-" "+")))
                          (setq acc (concat acc separator query ":" value))))))
                  '("--path="
                    "--language="
                    "--not-user=")
                  "")))
    (github-code-search-request code result exact
                                uniq)))

(defun github-code-search-load-code-result-at-point ()
  "Download and display full file for the code result at the current point."
  (interactive)
  (let ((item (get-text-property (point) 'github-code-search-item)))
    (github-code-search-download-code item
                                      (get-text-property (point)
                                                         'github-code-search-str))))
(defvar-local github-code-search--old-header-line nil)

;;;###autoload
(define-minor-mode github-code-search-results-mode
  "Minor mode for interacting for github code search results."
  :lighter " gh-search-results"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "+") #'github-code-search-next-page)
    (define-key map (kbd "e") #'github-code-search-toggle-exact)
    (define-key map (kbd ".") #'github-code-search-toggle-exact)
    (define-key map (kbd "u") #'github-code-search-toggle-uniq)
    (define-key map (kbd "g") #'github-code-search-revert)
    map)
  :global nil
  (if (not github-code-search-results-mode)
      (setq header-line-format github-code-search--old-header-line)
    (setq-local revert-buffer-function #'github-code-search-revert)
    (setq github-code-search--old-header-line header-line-format
          header-line-format
          (list (concat (propertize " " 'display '(space :align-to 0))
                        (format "%s" (buffer-name)))
                (string-join (delq nil
                                   (list (when github-code-search-uniq
                                           "[Uniq]")
                                         (when github-code-search-exact
                                           "[Exact]")))
                             " ")
                (propertize "Ready" 'face 'success)
                '(:eval
                  (let* ((num-exchanges
                          (format "[Next page %s]"
                                  (1+ github-code-search-page)))
                         (l2 (length num-exchanges)))
                    (concat
                     (propertize
                      " " 'display `(space :align-to ,(max 1 (- (window-width) (+ 2 l2)))))
                     (propertize
                      (button-buttonize num-exchanges
                                        'github-code-search-next-page)
                      'mouse-face (if (and github-code-search-total-count
                                           (> github-code-search-total-count
                                              (length github-code-search-response)))
                                      'highlight
                                    'font-lock-comment-face)
                      'help-echo
                      "Number of past exchanges to include with each request")
                     " ")))))))

(defun github-code-search--update-header-line (msg face)
  "Update header line with status MSG in FACE."
  (and (symbol-value 'github-code-search-results-mode)
       (consp header-line-format)
       (setf (nth 1 header-line-format)
             (propertize msg 'face face))
       (force-mode-line-update)))

(defun github-code-search-github-explorer (repo)
  "Retrieve and display the file structure of a specified GitHub repository.

Argument REPO is the name of the GitHub repository that the function will
interact with."
  (when (and (fboundp 'github-explorer-paths--put)
             (fboundp 'github-explorer--tree))
    (url-retrieve
     (format
      "https://api.github.com/repos/%s/git/trees/HEAD:?recursive=1"
      repo)
     (lambda (arg)
       (cond ((equal :error (car arg))
              (message arg))
             (t
              (with-current-buffer (current-buffer)
                (goto-char (point-min))
                (re-search-forward "^$")
                (delete-region (+ 1 (point))
                               (point-min))
                (goto-char (point-min))
                (let* ((paths
                        (remove nil
                                (mapcar
                                 (lambda (x)
                                   (if (equal
                                        (cdr
                                         (assoc
                                          'type x))
                                        "blob")
                                       (cdr (assoc 'path x))))
                                 (cdr (assoc 'tree
                                             (json-read)))))))
                  (github-explorer-paths--put repo paths)
                  (github-explorer--tree repo
                                         (format
                                          "https://api.github.com/repos/%s/git/trees/%s"
                                          repo "HEAD")
                                         "/")))))))))

(defun github-code-search-insert-item (item search-str)
  "Insert a GitHub code search ITEM into the current buffer.

Argument ITEM is an alist containing the details of a specific item from a
GitHub code search.
Argument SEARCH-STR is the string that was used to perform the GitHub code
search."
  (let* ((name (alist-get 'name item))
         (path (alist-get 'path item))
         (repository (alist-get 'repository item))
         (repo-url (alist-get 'html_url repository))
         (repo-name (alist-get 'full_name repository))
         (html_url (alist-get 'html_url item))
         (text-matches (alist-get 'text_matches item))
         (code (github-code-search-fontify (mapconcat
                                            #'github-code-search-render-text-match
                                            text-matches
                                            "\n\n")
                                           (or name path)))
         (title (concat (if repo-name (concat repo-name "/") "")
                        (or path name)))
         (buttons (delq nil (list
                             (when repo-url
                               (buttonize "Browse repo" #'browse-url repo-url))
                             (when (progn
                                     (require 'github-explorer nil t)
                                     (fboundp 'github-explorer))
                               (buttonize "Browse in Emacs "
                                          #'github-code-search-github-explorer
                                          repo-name))))))
    (let ((inhibit-read-only t))
      (insert (if (bobp)
                  ""
                "\n")
              (buttonize title #'browse-url html_url) "\n")
      (let ((beg (point))
            (end))
        (insert code)
        (setq end (point))
        (add-text-properties beg end
                             `(html_url ,html_url
                                        github-code-search-item ,item
                                        github-code-search-str ,search-str
                                        keymap ,github-code-search-src-keymap)))
      (when buttons
        (insert "\n" (string-join buttons "\s") "\n")))))

(defun github-code-search-insert-matches (items code)
  "Insert matches from a GitHub CODE search into the current buffer.

Argument ITEMS is a list of items that will be iterated over in the
function.

Argument CODE is the specific CODE that will be inserted for each item
in the items list and highlighted in the search results."
  (let ((inhibit-read-only t))
    (dolist (item items)
      (github-code-search-insert-item item code)
      (insert ?\n (make-string 80 ?_) ?\n ?\n ?\n))
    (github-code-search-higlight-matches code)))

(defun github-code-search-get-languages (str pred action)
  "Initialize and complete GitHub code search languages based on given parameters.

Argument STR is a string that is used as the input for the completion
function.
Argument PRED is a predicate function that filters the completion
candidates.
Argument ACTION is an ACTION to be performed on the completion
candidates."
  (setq github-code-search-langs-alist (or github-code-search-langs-alist
                                           (github-code-search-init-languages)))
  (setq github-code-search-langs
        (or github-code-search-langs
            (mapcan #'cdr (copy-tree
                           github-code-search-langs-alist))))
  (if (eq action 'metadata)
      nil
    (complete-with-action action github-code-search-langs str pred)))

;;;###autoload (autoload 'github-code-search "github-code-search" nil t)
(transient-define-prefix github-code-search ()
  "A menu for GitHub code search with specific options."
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
    :choices github-code-search-get-languages)
   ("p" "path" "--path=" :class transient-option)
   ("u" "user" "--user=" :class transient-option)]
  ["Filtering"
   ("U" "not-user" "--not-user=" :class transient-option)
   ("e" "Exact matches" "--exact")
   ("d" "Delete dublicates" "--uniq")]
  ["Actions"
   ("C-c C-o" "Browse" github-code-search-browse)
   ("RET" "Search" github-code-search-code)]
  (interactive)
  (transient-setup #'github-code-search))

(provide 'github-code-search)
;;; github-code-search.el ends here