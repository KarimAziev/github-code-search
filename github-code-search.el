;;; github-code-search.el --- Configure code search -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/github-code-search
;; Keywords: tools
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (transient "0.4.1") (ghub "3.6.0"))
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

;; You can edit keybinding by modifiyng `github-code-search-results-mode-map'.


;;; Code:

(require 'transient)
(require 'ghub)

(declare-function json-read-from-string "json")

(eval-and-compile)
(declare-function json-read "json")
(declare-function auth-source-forget "auth-source")
(declare-function auth-info-password "auth-source")
(declare-function auth-source-search "auth-source")

(defcustom github-code-search-ghub-auth-info '("" . github-code-search)
  "A cons of (USERNAME . TOKEN) or a function that returns it.
The cdr can be either a string containing the OAuth token,
or a symbol indicating where to fetch the OAuth token."
  :type '(radio
          (cons :tag "Cons cell"
                (string :tag "Github Username")
                (radio
                 :tag "Marker"
                 (symbol :tag "Suffix" github-code-search)
                 (string :tag "OAuth Token")))
          (function-item  :tag "Use gh cli"
                          github-code-search-auth-from-gh-config)
          (function :tag "Custom Function"))
  :group 'github-code-search)

(defcustom github-code-search-word-browse-fn 'browse-url
  "Function to browse results of code search."
  :type 'function
  :group 'github-code-search)


(defcustom github-code-search-per-page-limit 100
  "The number of results per page. It should be the value between 30 to 100."
  :type 'integer
  :group 'github-code-sarch)

(defvar github-code-search-spinner-text-elems ["◴" "◷" "◶" "◵"]
  "List of strings to show in sequence for the spinner.")

(defvar-local github-code-search-spinner-timer nil
  "Timer to update the spinner display.")

(defvar github-code-search--cached-auth-data nil)

(defvar github-code-search-langs-alist nil)


(defvar github-code-search-langs nil)
(defvar github-code-search-result-buffer-name "*github-code-search*")

(defvar-local github-code-search--search-code nil)
(defvar-local github-code-search--search-extra-query nil)
(defvar-local github-code-search-response nil)
(defvar-local github-code-search-exact nil)
(defvar-local github-code-search-total-count nil)
(defvar-local github-code-search-loading nil)
(defvar-local github-code-search-page 1)
(defvar-local github-code-search-uniq nil)


(defun github-code-search-read-auth-marker ()
  "Retrieve and optionally save GitHub auth credentials."
  (when-let ((variants
              (seq-uniq
               (auth-source-search
                :host "api.github.com"
                :require '(:user :secret)
                :max most-positive-fixnum)
               (lambda (a b)
                 (equal (auth-info-password a)
                        (auth-info-password b))))))
    (pcase-let* ((users (seq-filter (apply-partially #'string-match-p "\\^")
                                    (delq nil
                                          (mapcar
                                           (lambda (it)
                                             (when (plist-get it :user)
                                               (plist-get it :user)))
                                           variants))))
                 (user (if (> (length users) 1)
                           (completing-read
                            "Source:\s"
                            users
                            nil t)
                         (read-string "User: " (car users))))
                 (`(,user ,marker)
                  (split-string user "\\^" t))
                 (cell (cons user
                             (if marker
                                 (intern marker)
                               (read-string "Ghub token: ")))))
      (if (yes-or-no-p "Save for future?")
          (customize-save-variable 'github-code-search-ghub-auth-info cell
                                   "Saved by github-code-search")
        (setq github-code-search-ghub-auth-info cell))
      github-code-search-ghub-auth-info)))


(defun github-code-search--auth-source-get (keys &rest spec)
  "Retrieve authentication data for GitHub repositories.

Argument KEYS is a list of keys for which values are to be retrieved from
the authentication source.

Optional argument SPEC is a variable argument list that specifies the
search criteria for the authentication source."
  (declare (indent 1))
  (let ((plist (car (apply #'auth-source-search
                           (append spec (list :max 1))))))
    (mapcar (lambda (k)
              (plist-get plist k))
            keys)))

(defun github-code-search-auth (&optional force)
  "Authenticate a GitHub repository, optionally forcing a `re-authentication'.

Optional argument FORCE is a boolean.
If non-nil, it forces the function to re-authenticate by clearing the cached
authentication data.
The default value is nil."
  (require 'auth-source)
  (when force (setq github-code-search--cached-auth-data nil))
  (or github-code-search--cached-auth-data
      (setq github-code-search--cached-auth-data
            (pcase github-code-search-ghub-auth-info
              ((pred functionp)
               (funcall github-code-search-ghub-auth-info))
              (`((and ,username
                      (stringp ,username)
                      (not (string-empty-p ,username)))
                 .
                 (and ,marker
                      (symbolp ,marker)
                      ,marker))
               (let* ((user (format "%s^%s" username marker))
                      (token
                       (or (car (github-code-search--auth-source-get (list :secret)
                                                                     :host "api.github.com"
                                                                     :user user))
                           (auth-source-forget (list
                                                :host "api.github.com"
                                                :user user
                                                :max 1)))))
                 (cons username
                       (if (functionp token)
                           (funcall token)
                         token))))
              (`((and ,username
                      (stringp ,username)
                      (not (string-empty-p ,username)))
                 .
                 (and ,marker
                      (stringp ,marker)
                      ,marker))
               (cons username marker))
              (_ (github-code-search-read-auth-marker))))))

;;;###autoload
(defun github-code-search-change-user ()
  "Toggle GitHub code search user authentication."
  (interactive)
  (if github-code-search--cached-auth-data
      (setq github-code-search--cached-auth-data nil)
    (github-code-search-auth t))
  (when transient-current-command
    (transient-setup transient-current-command)))


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
    ((or 'css-ts-mode 'css-mode)
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
    ((or 'clojurescript-mode 'clojure-mode) "clojure")
    ('org-mode "org")))

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

(defun github-code-search-get-browse-query ()
  "Generate GitHub code search URL from arguments."
  (let* ((code (github-code-search-get-arg-value-from-args "code"))
         (path (github-code-search-get-arg-value-from-args "filename"))
         (query
          (replace-regexp-in-string "[+]" " "
                                    (github-code-search-format-args-to-query
                                     (seq-remove
                                      (apply-partially
                                       #'string-match-p
                                       "^--\\(filename\\|code\\)=")
                                      (github-code-search-get-args-for-query)))))
         (type
          (cond ((and path (or (not code)
                               (string-empty-p code)))
                 "path")
                (t "Code")))
         (q (string-join (list (if path
                                   (concat "path:" path " " (or code ""))
                                 code)
                               query)
                         " "))
         (query-url (github-code-search-query-from-alist
                     `(("q" ,q)
                       ("type" ,type)))))
    query-url))

;;;###autoload
(defun github-code-search-browse-gists ()
  "Search GitHub gists online."
  (interactive)
  (funcall github-code-search-word-browse-fn
           (concat "https://gist.github.com/search?"
                   (github-code-search-get-browse-query))))

;;;###autoload
(defun github-code-search-browse ()
  "Search GitHub code via a web browser."
  (interactive)
  (funcall github-code-search-word-browse-fn
           (concat "https://github.com/search?"
                   (github-code-search-get-browse-query))))



(defun github-code-search-auth-from-gh-config ()
  "Extract and return GitHub username and OAuth token from gh config file."
  (pcase-let* ((`(,username . ,file)
                (with-temp-buffer
                  (when (zerop
                         (call-process
                          "gh" nil t
                          nil "auth" "status"))
                    (re-search-backward
                     "Logged in to github.com as \\([a-zA-Z0-9-]*[a-zA-Z0-9]\\{1\\}\\)[\s\t\n]+[(]\\([^)]+\\)[)]"
                     nil t 1)
                    (cons (match-string-no-properties 1)
                          (match-string-no-properties 2)))))
               (token (and username file
                           (file-exists-p file)
                           (with-temp-buffer
                             (insert-file-contents file)
                             (when (re-search-forward
                                    "github\\.com:[\s\n]+oauth_token:[\s]+\\([^\n]+\\)"
                                    nil t 1)
                               (match-string-no-properties
                                1))))))
    (and token (cons username token))))

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

(defun github-code-search-get-status-error (status)
  "Display error details from GitHub code search status.

Argument STATUS is a plist containing the status information, including any
error details."
  (when-let ((err (plist-get status :error)))
    (concat (propertize
             "github-code-search error: "
             'face
             'error)
            (mapconcat (apply-partially #'format "%s")
                       (delq nil
                             (list (or
                                    (when-let ((type
                                                (ignore-errors
                                                  (cadr
                                                   err))))
                                      type)
                                    err)
                                   (ignore-errors (caddr
                                                   err))
                                   (ignore-errors
                                     (alist-get 'message
                                                (car-safe
                                                 (last
                                                  err))))
                                   (ignore-errors
                                     (alist-get 'documentation_url
                                                (car-safe
                                                 (last
                                                  err))))))
                       " "))))
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
  (let ((q (string-join (delq nil
                              (list code query))
                        "")))
    (ghub-request "GET"
                  (concat "search/code?q=" q
                          (format "&per_page=%s&page=%s"
                                  (or
                                   (when-let* ((buff
                                                (get-buffer
                                                 github-code-search-result-buffer-name))
                                               (total (buffer-local-value
                                                       'github-code-search-total-count
                                                       buff))
                                               (len
                                                (length (buffer-local-value
                                                         'github-code-search-response
                                                         buff))))
                                     (when (> total len)
                                       (min (- total len)
                                            github-code-search-per-page-limit)))
                                   github-code-search-per-page-limit)
                                  page))
                  nil
                  :auth (cdr github-code-search--cached-auth-data)
                  :username (car github-code-search--cached-auth-data)
                  :headers
                  `(("Accept" . "application/vnd.github.text-match+json"))
                  :forge 'github
                  :host "api.github.com"
                  :callback
                  (lambda (value _headers status _req)
                    (let ((buffer (get-buffer
                                   github-code-search-result-buffer-name)))
                      (when-let ((err
                                  (github-code-search-get-status-error
                                   status)))
                        (message err))
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
                          (ignore-errors (funcall callback value)))))))))

(defun github-code-search-next-page (&rest _)
  "Increment the page number and request the next page of GitHub code search."
  (interactive)
  (cond ((and
          (not github-code-search-loading)
          github-code-search-total-count
          (> github-code-search-total-count
             (length github-code-search-response)))
         (setq github-code-search-page (1+ github-code-search-page)
               github-code-search-loading t)
         (github-code-search-request github-code-search--search-code
                                     github-code-search--search-extra-query
                                     github-code-search-exact
                                     github-code-search-uniq))))

(defun github-code-search-get-pagination-button-bounds ()
  "Locate pagination button text boundaries."
  (pcase-let ((`(,beg . ,end)
               (save-excursion
                 (goto-char (point-min))
                 (when-let ((prop (text-property-search-forward
                                   'github-code-search-pagination-button t
                                   'equal)))
                   (cons (prop-match-beginning prop)
                         (prop-match-end prop))))))
    (when beg
      (cons beg end))))

(defun github-code-search-delete-footer ()
  "Remove the footer from the GitHub code search results."
  (require 'text-property-search)
  (let ((inhibit-read-only t))
    (save-excursion
      (pcase-let ((`(,beg . ,end)
                   (github-code-search-get-pagination-button-bounds)))
        (when beg
          (delete-region beg end))))))



(defun github-code-search-spinner-find-overlay ()
  "Locate the spinner overlay in a buffer."
  (seq-find (lambda (o)
              (and (overlayp o)
                   (overlay-get o 'github-code-search-spinner-overlay)))
            (car (overlay-lists))))

(defun github-code-search-spinner-cleanup ()
  "Remove the spinner overlay."
  (when-let ((ov (github-code-search-spinner-find-overlay)))
    (delete-overlay ov)))

(defun github-code-search-spinner-overlay-make (start end &optional buffer front-advance
                                      rear-advance &rest props)
  "Create a new overlay with range BEG to END in BUFFER and return it.

If omitted, BUFFER defaults to the current buffer.
START and END may be integers or markers.
The fourth arg FRONT-ADVANCE, if non-nil, makes the marker
for the front of the overlay advance when text is inserted there
\(which means the text *is not* included in the overlay).
The fifth arg REAR-ADVANCE, if non-nil, makes the marker
for the rear of the overlay advance when text is inserted there
\(which means the text *is* included in the overlay).
PROPS is a plist to put on overlay."
  (let ((overlay (make-overlay start end buffer front-advance
                               rear-advance)))
    (dotimes (idx (length props))
      (when (eq (logand idx 1) 0)
        (let* ((prop-name (nth idx props))
               (val (plist-get props prop-name)))
          (overlay-put overlay prop-name val))))
    overlay))


(defun github-code-search-spinner--stop ()
  "Stop the active spinner timer."
  (when (timerp github-code-search-spinner-timer)
    (cancel-timer github-code-search-spinner-timer)
    (setq github-code-search-spinner-timer nil)))


(defun github-code-search-spinner-update (buffer)
  "Update spinner animation in a specified buffer.

Argument BUFFER is the buffer where the spinner update should occur."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let ((wnd (get-buffer-window buffer)))
        (with-selected-window wnd
          (pcase-let* ((`(,beg . ,end)
                        (github-code-search-get-pagination-button-bounds))
                       (ov
                        (or (github-code-search-spinner-find-overlay)
                            (and end
                                 (github-code-search-spinner-overlay-make (1-
                                                                           end)
                                                                          (1-
                                                                           end)
                                                                          (current-buffer)
                                                                          t
                                                                          nil
                                                                          'github-code-search-spinner-overlay
                                                                          t
                                                                          'before-string
                                                                          (aref
                                                                           github-code-search-spinner-text-elems
                                                                           0)
                                                                          'help-echo
                                                                          "Loading..."))))
                       (next-idx
                        (mod (or
                              (ignore-errors
                                (+ 1
                                   (seq-position
                                    github-code-search-spinner-text-elems
                                    (replace-regexp-in-string
                                     (concat "[^" (mapconcat
                                                   'identity
                                                   github-code-search-spinner-text-elems
                                                   "")
                                             "]")
                                     ""
                                     (overlay-get ov 'before-string)))))
                              0)
                             (length github-code-search-spinner-text-elems)))
                       (next-str (aref github-code-search-spinner-text-elems
                                       next-idx)))
            (if (and beg end ov)
                (progn (move-overlay ov (1- end)
                                     (1- end) buffer)
                       (overlay-put ov 'before-string next-str))
              (github-code-search-spinner--stop)
              (github-code-search-spinner-cleanup))))))))


;;;###autoload
(defun github-code-search-spinner-start ()
  "Start a repeating timer to update a spinner."
  (interactive)
  (github-code-search-spinner--stop)
  (github-code-search-spinner-cleanup)
  (setq github-code-search-spinner-timer
        (run-with-timer 0.2 0.2 'github-code-search-spinner-update (current-buffer))))

;;;###autoload
(defun github-code-search-spinner-stop ()
  "Stop the spinner and clean up resources."
  (interactive)
  (github-code-search-spinner--stop)
  (github-code-search-spinner-cleanup))


(defun github-code-search-property-boundaries (prop &optional pos)
  "Return boundaries of property PROP at POS (cdr is 1+)."
  (unless pos (setq pos (point)))
  (let (beg end val)
    (setq val (get-text-property pos prop))
    (if (null val)
        val
      (if (or (bobp)
              (not (eq (get-text-property (1- pos) prop) val)))
          (setq beg pos)
        (setq beg (previous-single-property-change pos prop))
        (when (null beg)
          (setq beg (point-min))))
      (if (or (eobp)
              (not (eq (get-text-property (1+ pos) prop) val)))
          (setq end pos)
        (setq end (next-single-property-change pos prop))
        (when (null end)
          (setq end (point-max))))
      (cons beg end))))

(defun github-code-search-toggle-button-at-point (&optional btn &rest _)
  "Toggle pagination button and fetch next GitHub code search page.

Optional argument BTN is a marker pointing to the button to toggle. If nil, the
point is used."
  (pcase-let
      ((`(,beg . ,end)
        (github-code-search-property-boundaries
         'github-code-search-pagination-button
         (if btn
             (marker-position btn)
           (point))))
       (inhibit-read-only t))
    (when (and beg end)
      (cond ((eq (face-at-point) 'button)
             (add-face-text-property beg end 'font-lock-comment-face nil)
             (github-code-search-spinner-start)
             (github-code-search-next-page))))))

(defun github-code-search-get-button ()
  "Create a \"Next Page\" button for GitHub code search results."
  (let* ((next-page
          (when (and github-code-search-total-count
                     github-code-search-page
                     (> github-code-search-total-count
                        (length github-code-search-response)))
            (1+ github-code-search-page)))
         (label (format "[Next page %s]"
                        (cond (next-page next-page)
                              (t "None"))))
         (btn (buttonize label #'github-code-search-toggle-button-at-point))
         (len (length btn)))
    (add-text-properties 0 len
                         '(github-code-search-pagination-button t)
                         btn)
    (when (or (not next-page)
              github-code-search-loading)
      (add-face-text-property 0 len 'font-lock-comment-face nil
                              btn))
    btn))



(defun github-code-search-render-footer ()
  "Render the footer for the GitHub code search page."
  (github-code-search-spinner--stop)
  (github-code-search-spinner-cleanup)
  (github-code-search-delete-footer)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (let ((beg)
            (end (point)))
        (while (looking-back "\n\n" 0)
          (forward-line -1)
          (setq beg (point)))
        (when beg
          (delete-region beg end)))
      (newline)
      (insert
       (github-code-search-get-button)))))

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


(defun github-code-search-revert (&rest _)
  "Revert the GitHub code search buffer and update it with new search results."
  (let ((filtered github-code-search-response))
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
      (github-code-search-update-header-line)
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
      (setq github-code-search-loading t)
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
      (unless (derived-mode-p 'github-code-search-result-mode)
        (github-code-search-result-mode))
      (github-code-search-update-header-line)
      (github-code-search-spinner-start)
      (setq github-code-search--search-code code)
      (setq github-code-search--search-extra-query query)
      (github-code-search-async-request
       code query github-code-search-page
       (lambda (value)
         (let* ((items (alist-get 'items value))
                (filtered-items items))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (setq github-code-search-loading nil)
               (setq github-code-search-exact exact)
               (when (zerop (length (alist-get 'items value)))
                 (setq github-code-search-page (max 1 (1- github-code-search-page))))
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
                 (github-code-search-update-header-line)))
             (unless (get-buffer-window buffer)
               (pop-to-buffer buffer)
               (select-window (get-buffer-window buffer))))))))))

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
        (delay-mode-hooks         (set-auto-mode)
                                  (font-lock-ensure))))
    (buffer-string)))

(defun github-code-search-download-code (item &optional search-str)
  "Download and display GitHub code based on a provided ITEM and search string.

Argument ITEM is an association list that contains information about a specific
item from a GitHub code search.
Argument SEARCH-STR is an optional string that specifies the text to search for
within the downloaded code."
  (let* ((data (github-code-search-json-parse-string
                (github-code-search-download-url (alist-get 'url item))
                'alist
                'list))
         (github-file (or (alist-get 'name item)
                          (alist-get 'path item)))
         (file (expand-file-name github-file
                                 default-directory))
         (code (decode-coding-string (base64-decode-string (alist-get 'content
                                                                      data))
                                     'utf-8)))
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
  (github-code-search-auth)
  (let* ((args (transient-args transient-current-command))
         (code (github-code-search-get-arg-value-from-args "code"))
         (query (github-code-search-format-args-to-query
                 (seq-remove (apply-partially #'string-match-p "^--\\(code=\\)")
                             (github-code-search-get-args-for-query))))
         (exact (transient-arg-value "--exact" args))
         (uniq (transient-arg-value "--uniq" args)))
    (github-code-search-request code query exact
                                uniq)))

(defun github-code-search-load-code-result-at-point ()
  "Download and display full file for the code result at the current point."
  (interactive)
  (let ((item (get-text-property (point) 'github-code-search-item)))
    (github-code-search-download-code item
                                      (get-text-property (point)
                                                         'github-code-search-str))))
(defvar-local github-code-search--old-header-line nil)

(defun github-code-search-update-header-line ()
  "Update header line with GitHub code search info."
  (when (derived-mode-p 'github-code-search-result-mode)
    (setq header-line-format
          (list (concat (propertize " " 'display '(space :align-to 0))
                        (format "%s " (buffer-name)))
                '(:eval (propertize (format (concat " Page %s (%s/%s) ")
                                            (or github-code-search-page 1)
                                            (length github-code-search-response)
                                            (or github-code-search-total-count 0))
                                    'face
                                    'font-lock-number-face))
                (if github-code-search-loading
                    (propertize " Loading" 'face 'warning)
                  (propertize " Ready" 'face 'success))
                (string-join (delq nil
                                   (list (when github-code-search-uniq
                                           "[Uniq]")
                                         (when github-code-search-exact
                                           "[Exact]")))
                             " ")
                (let* ((btn (github-code-search-get-button))
                       (l2 (length btn)))
                  (concat
                   (propertize
                    " " 'display `(space :align-to ,(max 1 (- (window-width) (+ 2 l2)))))
                   btn
                   " "))))))

(defvar github-code-search-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "+") #'github-code-search-next-page)
    (define-key map (kbd "e") #'github-code-search-toggle-exact)
    (define-key map (kbd ".") #'github-code-search-toggle-exact)
    (define-key map (kbd "u") #'github-code-search-toggle-uniq)
    (define-key map (kbd "?") #'github-code-search)
    map))



(define-derived-mode github-code-search-result-mode special-mode
  "Github Code Search"
  "Major mode for interacting with Github code search results.

The following commands are available:

\\<github-code-search-results-mode-map>
\\{github-code-search-results-mode-map}."
  (setq-local buffer-undo-list t)
  (setq-local text-scale-remap-header-line t)
  (setq github-code-search--old-header-line header-line-format)
  (when (fboundp 'header-line-indent-mode)
    (header-line-indent-mode))
  (github-code-search-update-header-line)
  (setq-local revert-buffer-function #'github-code-search-revert)
  (let ((map (copy-keymap github-code-search-results-mode-map)))
    (set-keymap-parent map (make-composed-keymap button-buffer-map
                                                 special-mode-map))
    (use-local-map map)))

(put 'github-code-search-result-mode 'mode-class 'special)



(defun github-code-search-json-read-buffer (&optional object-type array-type null-object
                                    false-object)
  "Parse json from the current buffer using specified object and array types.

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
      (json-parse-buffer
       :object-type (or object-type 'alist)
       :array-type
       (pcase array-type
         ('list 'list)
         ('vector 'array)
         (_ 'array))
       :null-object (or null-object :null)
       :false-object (or false-object :false))
    (let ((json-object-type (or object-type 'alist))
          (json-array-type
           (pcase array-type
             ('list 'list)
             ('array 'vector)
             (_ 'vector)))
          (json-null (or null-object :null))
          (json-false (or false-object :false)))
      (json-read))))

(defun github-code-search-fetch-git-tree (repo &optional callback)
  "Fetch the git tree of a specified GitHub repository.

Argument REPO is a string that represents the repository for which the git tree
is to be fetched.

Optional argument CALLBACK is a function that will be called with the fetched
git tree as its argument.
If not provided, the function will return the git tree directly."
  (url-retrieve
   (format "https://api.github.com/repos/%s/git/trees/HEAD:?recursive=1" repo)
   (lambda (arg)
     (if-let ((err (plist-get arg :error)))
         (minibuffer-message (concat
                              (format (propertize
                                       "Error fetching: " 'face
                                       'error)
                                      repo)
                              (format "%s " (or
                                             (when-let ((type
                                                         (ignore-errors
                                                           (cadr
                                                            err))))
                                               type)
                                             err))
                              (or
                               (when-let ((status (ignore-errors (caddr err))))
                                 (format "%s" status))
                               "")))
       (with-current-buffer (current-buffer)
         (goto-char (point-min))
         (re-search-forward "^$")
         (delete-region (+ 1 (point))
                        (point-min))
         (goto-char (point-min))
         (let ((tree (cdr (assq 'tree (github-code-search-json-read-buffer)))))
           (if callback
               (funcall callback tree)
             tree)))))))

(defun github-code-search-github-explorer (repo)
  "Retrieve and display the file structure of a specified GitHub repository.

Argument REPO is the name of the GitHub repository that the function will
interact with."
  (when (and (fboundp 'github-explorer-paths--put)
             (fboundp 'github-explorer--tree))
    (github-code-search-fetch-git-tree
     repo
     (lambda (tree)
       (let ((paths
              (delq nil
                    (mapcar
                     (lambda (x)
                       (when (equal (cdr (assq 'type x)) "blob")
                         (cdr (assoc 'path x))))
                     tree))))
         (github-explorer-paths--put repo
                                     paths)
         (github-explorer--tree repo
                                (format
                                 "https://api.github.com/repos/%s/git/trees/%s"
                                 repo "HEAD")
                                "/"))))
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
                (let*
                    ((paths
                      (remove nil
                              (mapcar
                               (lambda (x)
                                 (if (equal
                                      (cdr
                                       (assoc
                                        'type x))
                                      "blob")
                                     (cdr (assoc 'path x))))
                               (cdr
                                (assoc 'tree
                                       (github-code-search-json-read-buffer)))))))
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
                                            (apply-partially #'alist-get
                                                             'fragment)
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

(defun github-code-search-queries-to-options (queries)
  "Convert GitHub code search QUERIES into command line options.

Argument QUERIES is a list."
  (let* ((props-alist
          '(("language" :choices
             github-code-search-get-languages)
            ("code"
             :class transient-option
             :always-read t)))
         (items (mapcan
                 (lambda (v)
                   (let ((k (substring-no-properties v 0 1)))
                     (list (append (list (format "%s" k) v (format
                                                            "--%s=" v))
                                   (cdr (assoc-string v
                                                      props-alist)))
                           (append (list (format "%s" (upcase k))
                                         (format "not %s" v)
                                         (format "--not-%s=" v))
                                   (cdr (assoc-string v
                                                      props-alist))))))
                 queries)))
    items))

(defun github-code-search-make-incompatible-queries (queries)
  "Generate incompatible QUERIES for GitHub code search.

Argument QUERIES is a list of strings."
  (mapcar (lambda (it)
            (list (format "--%s=" it) (format "--not-%s=" it)))
          queries))

(defvar github-code-search-code-queries '("language"
                                          "filename"
                                          "user"
                                          "extension"
                                          "in"
                                          "org"))


(defun github-code-search-get-args-for-query ()
  "Remove specific arguments from the GitHub code search query."
  (seq-remove
   (apply-partially  #'string-match-p "^--\\(exact\\|uniq\\)")
   (github-code-search-get-arguments)))

(defun github-code-search-get-arg-value-from-args (arg)
  "Extract the ARG value from the argument list in a GitHub code search."
  (when-let ((argument (seq-find
                        (apply-partially #'string-match-p
                                         (concat "--"
                                                 (regexp-quote
                                                  arg)
                                                 "="))
                        (github-code-search-get-args-for-query))))
    (let ((parts
           (split-string argument "=" t)))
      (string-join (seq-drop parts 1) "="))))

(defun github-code-search-query-description ()
  "Formats a GitHub code search query based on specified arguments."
  (format "search/code?q=%s"
          (github-code-search-format-args-to-query
           (github-code-search-get-args-for-query))))

(defun github-code-search-get-arguments ()
  "Return current transient arguments ARGS."
  (let ((raw-args))
    (cond (transient-current-command
           (setq raw-args (transient-args transient-current-command)))
          (transient--prefix
           (setq transient-current-prefix transient--prefix)
           (setq transient-current-command (oref transient--prefix command))
           (setq transient-current-suffixes transient--suffixes)
           (setq raw-args (transient-args transient-current-command))))
    raw-args))

(defun github-code-search-format-args-to-query (args)
  "Generate a GitHub code search query from given arguments.

Argument ARGS is a list of strings, each representing a search argument."
  (seq-reduce
   (lambda (acc argument)
     (let* ((parts
             (split-string argument "=" t))
            (arg (pop parts))
            (value (string-join parts "=")))
       (if (not value)
           acc
         (let* ((neg (string-prefix-p "--not-" arg))
                (query (if neg
                           (replace-regexp-in-string
                            "\\(^--not-\\)" ""
                            arg)
                         (replace-regexp-in-string "^--" ""
                                                   arg)))
                (separator (if neg "+-" "+")))
           (setq acc (concat acc separator query ":" value))))))
   args
   ""))


;;;###autoload (autoload 'github-code-search "github-code-search" nil t)
(transient-define-prefix github-code-search ()
  "A menu for GitHub code search with specific options."
  :incompatible (github-code-search-make-incompatible-queries
                 github-code-search-code-queries)
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
                     (concat "--filename=" "*." ext)
                   (when (and buffer-file-name (not code-arg))
                     (concat "--filename="
                             (file-name-nondirectory buffer-file-name)))))))))
  [:description github-code-search-query-description
                :setup-children
                (lambda (&rest _argsn)
                  (mapcar
                   (apply-partially #'transient-parse-suffix
                                    transient--prefix)
                   (append (list
                            (list "c" "code" "--code="
                                  :class 'transient-option
                                  :always-read t))
                           (github-code-search-queries-to-options
                            github-code-search-code-queries))))]
  ["Filtering"
   ("x" "Exact matches" "--exact")
   ("D" "Delete dublicates" "--uniq")]
  ["Config"
   ("*" github-code-search-change-user
    :description (lambda ()
                   (concat  "Github User: "
                            (if
                                (or (not (car-safe
                                          github-code-search--cached-auth-data))
                                    (string-empty-p
                                     (car
                                      github-code-search--cached-auth-data))
                                    (not (cdr-safe
                                          github-code-search--cached-auth-data)))
                                "(not logged)"
                              (propertize
                               (substring-no-properties
                                (or (car-safe
                                     github-code-search--cached-auth-data)
                                    ""))
                               'face 'transient-value)))))]
  ["Actions"
   ("g g" "Browse" github-code-search-browse)
   ("g i" "github-code-search-browse-gists" github-code-search-browse-gists)
   ("RET" "Search" github-code-search-code)]
  (interactive)
  (transient-setup #'github-code-search))

(provide 'github-code-search)
;;; github-code-search.el ends here