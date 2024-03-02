;;; github-code-search.el --- Configure code search -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/github-code-search
;; Keywords: tools
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (transient "0.5.3") (ghub "3.6.0"))
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
;; | `?'   | Show transient menu with actions             |

;; You can edit keybinding by modifiyng `github-code-search-results-mode-map'.


;;; Code:

(require 'transient)
(require 'ghub)

(declare-function json-read-from-string "json")
(declare-function json-read "json")
(declare-function auth-source-forget "auth-source")
(declare-function auth-info-password "auth-source")
(declare-function auth-source-search "auth-source")

(defcustom github-code-search-ghub-auth-info '("" . github-code-search)
  "Authentication data for GitHub code search.

Stores authentication information for GitHub code search.

The value is a cons cell, function symbol, or function that returns a cons cell
containing the GitHub username and either an OAuth token or a symbol indicating
a suffix for the username.

The cons cell should have the GitHub username as its car and either an OAuth
token as a string or a symbol as its cdr. If the cdr is a symbol, it should be
`github-code-search', which indicates that the username should be suffixed with
this symbol to form the user token.

Alternatively, the value can be a function symbol or a function that returns the
required cons cell. The predefined function
`github-code-search-auth-from-gh-config' can be used to extract the username and
OAuth token from the GitHub CLI configuration.

To use this variable, set it to the desired cons cell, function symbol, or
function. When the GitHub code search functionality requires authentication, it
will use the information provided by this variable."
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

(defcustom github-code-search-code-queries '(""
                                             "By language"
                                             ("language"
                                              :choices
                                              github-code-search-languages-choices
                                              :always-read t
                                              :class transient-option)
                                             ""
                                             "By file or directory"
                                             ("filename"
                                              :class transient-option)
                                             ("extension"
                                              :class transient-option)
                                             ("path"
                                              :class transient-option)
                                             ""
                                             "By owner"
                                             ("org"
                                              :incompatible ("user")
                                              :class transient-option)
                                             ("user"
                                              :incompatible ("org")
                                              :class transient-option))
  "List of query options for GitHub code search.

A list of query specifications for GitHub code search. Each element can be
either a string representing a separator or a cons cell where the car is a
string representing a GitHub query and the cdr is a property list of transient
properties.

The property list can contain transient options such as :choices, :always-read,
:class, and :incompatible, which define the behavior of the corresponding
transient command.

The default value is a list of predefined queries and separators that facilitate
searching by language, file or directory, and owner (organization or user).

Each query specification should be structured as follows:

- A string for a separator, which is used to visually separate groups of related
options in the transient interface.

- A cons cell where the car is a string representing the GitHub query parameter
\\=(e.g., \"language\", \"filename\", \"extension\", \"path\", \"org\",
\"user\") and the cdr is a property list with transient properties that
customize the behavior of the option.

Addionally the transient properties may include:

- :incompatible, which lists other options that are incompatible with the
current one.

This customization is part of the `github-code-search' group and should be
edited using the customization interface or by setting the variable directly in
Emacs Lisp code."
  :group 'github-code-search
  :type '(repeat
          (choice
           (cons
            (string :tag "Github Query")
            (plist
             :tag "Transient properties"
             :key-type symbol
             :value-type sexp))
           (string :tag "Separator"))))

(defcustom github-code-search-word-browse-fn 'browse-url
  "Function to call for browsing GitHub code search results.

Specifies the function to use for browsing GitHub code search results. The
default function is `browse-url', which opens the search results in the default
web browser.

The value should be a function that takes a single string argument, which is the
URL to open. This function will be called when executing a GitHub code search to
display the results in a web browser.

To change the browsing function, set this variable to the name of the desired
function. For example, to use a different browser or a custom browsing
mechanism, define a new function that takes a URL and opens it according to the
custom specifications, then set this variable to that function's name."
  :type 'function
  :group 'github-code-search)

(defcustom github-code-search-per-page-limit 100
  "Maximum results per page in GitHub code search.

Specifies the maximum number of search results to display per page when using
the GitHub code search API. The default value is 100.

The value must be an integer that represents the number of results to fetch per
page. Lowering this number may improve performance on slower connections, while
increasing it can reduce the number of API requests needed to retrieve all
results for a search query. However, GitHub's API may impose its own limits on
the maximum number of results per page, so setting this value too high may have
no effect."
  :type 'integer
  :group 'github-code-sarch)

(defvar github-code-search-paths-hash (make-hash-table :test 'equal)
  "Hash table mapping code search paths to their results.")

(defun github-code-search--paths-put (repo paths)
  "Store PATHS in a hash table with REPO as the key.

Argument REPO is the repository identifier for which PATHS are being stored.

Argument PATHS is a list of directory paths associated with the REPO."
  (puthash repo paths github-code-search-paths-hash))

(defun github-code-search--paths-get (repo)
  "Retrieve stored paths for a given REPO.

Argument REPO is the repository name for which to retrieve the code search
paths."
  (gethash repo github-code-search-paths-hash))

(defun github-code-search-current-buffer-repo ()
  "Search GitHub code in the current buffer's repository."
  (car (github-code-search-tree--current-repo-path)))

(defvar github-code-search-spinner-text-elems ["◴" "◷" "◶" "◵"]
  "Array of spinner animation characters for code search display.")

(defvar-local github-code-search-spinner-timer nil
  "Timer to update the spinner display.")

(defvar github-code-search--cached-auth-data nil
  "Stores cached authentication data for GitHub code searches.")

(defvar github-code-search-langs-alist nil
  "Alist mapping languages to their GitHub search keywords.")

(defvar-local github-code-search-initial-value nil)

(defvar github-code-search-langs nil
  "List of programming languages supported by GitHub code search.")

(defvar github-code-search-result-buffer-name "*github-code-search*"
  "Buffer name for displaying GitHub code search results.")

(defvar github-code-search-file-buffer-name-prefix "*github-code-search*: "
  "Prefix for buffer names used in GitHub code search results.")

(defvar-local github-code-search--search-code nil)
(defvar-local github-code-search--search-extra-query nil)
(defvar-local github-code-search-response nil)
(defvar-local github-code-search-total-count nil)
(defvar-local github-code-search-loading nil)
(defvar-local github-code-search-error nil)
(defvar-local github-code-search-page 1)
(defvar-local github-code-search-uniq nil)
(defvar-local github-code-search-exact nil)

(defmacro github-code-search-window-with-other-window (&rest body)
  "Execute BODY in other window.
If other window doesn't exists, split selected window right."
  `(with-selected-window
       (github-code-search--get-other-wind)
     (progn ,@body)))

(defun github-code-search-minibuffer-get-metadata ()
  "Retrieve metadata for minibuffer completion."
  (completion-metadata
   (buffer-substring-no-properties
    (minibuffer-prompt-end)
    (max (minibuffer-prompt-end)
         (point)))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun github-code-search-minibuffer-ivy-selected-cand ()
  "Return selected candidate's metadata and expanded filename."
  (when (and (memq 'ivy--queue-exhibit post-command-hook)
             (boundp 'ivy-text)
             (boundp 'ivy--length)
             (boundp 'ivy-last)
             (fboundp 'ivy--expand-file-name)
             (fboundp 'ivy-state-current))
    (cons
     (completion-metadata-get (ignore-errors
                                (github-code-search-minibuffer-get-metadata))
                              'category)
     (ivy--expand-file-name
      (if (and (> ivy--length 0)
               (stringp (ivy-state-current ivy-last)))
          (ivy-state-current ivy-last)
        ivy-text)))))

(defun github-code-search-minibuffer-get-default-candidates ()
  "Fetch default completion candidates in minibuffer."
  (when (minibufferp)
    (let* ((all (completion-all-completions
                 (minibuffer-contents)
                 minibuffer-completion-table
                 minibuffer-completion-predicate
                 (max 0 (- (point)
                           (minibuffer-prompt-end)))))
           (last (last all)))
      (when last (setcdr last nil))
      (cons
       (completion-metadata-get (github-code-search-minibuffer-get-metadata)
                                'category)
       all))))

(defun github-code-search-get-minibuffer-get-default-completion ()
  "Retrieve default completion for minibuffer content."
  (when (and (minibufferp) minibuffer-completion-table)
    (pcase-let* ((`(,category . ,candidates)
                  (github-code-search-minibuffer-get-default-candidates))
                 (contents (minibuffer-contents))
                 (top (if (test-completion contents
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate)
                          contents
                        (let ((completions (completion-all-sorted-completions)))
                          (if (null completions)
                              contents
                            (concat
                             (substring contents
                                        0 (or (cdr (last completions)) 0))
                             (car completions)))))))
      (cons category (or (car (member top candidates)) top)))))

(defvar github-code-search-minibuffer-targets-finders
  '(github-code-search-minibuffer-ivy-selected-cand
    github-code-search-get-minibuffer-get-default-completion)
  "List of functions to find minibuffer completion targets.")

(defun github-code-search-minibuffer-get-current-candidate ()
  "Retrieve the current candidate from the minibuffer."
  (let (target)
    (run-hook-wrapped
     'github-code-search-minibuffer-targets-finders
     (lambda (fun)
       (when-let ((result (funcall fun)))
         (when (and (cdr-safe result)
                    (stringp (cdr-safe result))
                    (not (string-empty-p (cdr-safe result))))
           (setq target result)))
       (and target (minibufferp))))
    target))

(defun github-code-search-minibuffer-web-restore-completions-wind ()
  "Resize *Completions* window after next completion."
  (when (eq this-command 'minibuffer-next-completion)
    (remove-hook 'post-command-hook
                 #'github-code-search-minibuffer-web-restore-completions-wind)
    (when-let ((win (get-buffer-window "*Completions*" 0)))
      (fit-window-to-buffer win completions-max-height))))

(defun github-code-search-minibuffer-action-no-exit (action)
  "Invoke ACTION on current minibuffer candidate without closing.

Argument ACTION is a function to be called with the current minibuffer candidate
as its argument."
  (pcase-let ((`(,_category . ,current)
               (github-code-search-minibuffer-get-current-candidate)))
    (when-let ((win (get-buffer-window "*Completions*" 0)))
      (minimize-window win)
      (add-hook 'post-command-hook
                #'github-code-search-minibuffer-web-restore-completions-wind))
    (with-minibuffer-selected-window
      (funcall action current))))

(defun github-code-search-minibuffer-preview-file ()
  "Preview a GitHub repo file without exiting minibuffer."
  (interactive)
  (github-code-search-minibuffer-action-no-exit
   #'github-code-search-preview-repo-file-action))

(defvar github-code-search-minibuffer-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j")
                #'github-code-search-minibuffer-preview-file)
    map)
  "Keymap for minibuffer file search with preview function.")

(defun github-code-search-completing-read-with-keymap (prompt collection
                                                              &optional keymap setup-fn predicate require-match initial-input hist
                                                              def inherit-input-method)
  "PROMPT for input with completion, optional KEYMAP, and setup function.

Argument PROMPT is a string to prompt the user.

Argument COLLECTION is a list of strings or an alist from which the user can
choose.

Optional argument KEYMAP is a keymap to use while reading from the minibuffer.

Optional argument SETUP-FN is a function to call before reading from the
minibuffer.

Optional argument PREDICATE is a function to filter the choices in COLLECTION.

Optional argument REQUIRE-MATCH is a boolean; if non-nil, the user is required
to select an existing entry in COLLECTION.

Optional argument INITIAL-INPUT is a string to prefill the minibuffer with.

Optional argument HIST is a symbol representing a minibuffer history list.

Optional argument DEF is the default value to return if the user enters an empty
string.

Optional argument INHERIT-INPUT-METHOD is a boolean; if non-nil, the minibuffer
inherits the current input method."
  (let ((collection (if (stringp (car-safe collection))
                        (copy-tree collection)
                      collection)))
    (minibuffer-with-setup-hook
        (lambda ()
          (when (minibufferp)
            (when keymap
              (let ((map (make-composed-keymap keymap
                                               (current-local-map))))
                (use-local-map map)))
            (when setup-fn
              (funcall setup-fn))))
      (completing-read prompt
                       collection
                       predicate
                       require-match initial-input hist
                       def inherit-input-method))))

(defun github-code-search-preview-repo-file-action (path)
  "Preview and download a file from a GitHub repository.

Argument PATH is a string representing the path within the repository to
preview."
  (let* ((repo (github-code-search-current-buffer-repo))
         (tree (github-code-search--paths-get repo))
         (url (cdr (assq 'url (seq-find (lambda (it)
                                          (equal path (cdr (assq 'path it))))
                                        tree)))))
    (github-code-search-download-repo-path
     repo
     path
     url)))

(defun github-code-search-kill-repo-buffers (&optional repo)
  "Close buffers related to GitHub code search.

Optional argument REPO is a string representing the repository name. If
provided, only buffers related to this repository will be killed."
  (interactive)
  (dolist (buff (buffer-list))
    (when (buffer-local-value 'github-code-search-file-mode buff)
      (if repo
          (when (string-prefix-p (concat
                                  github-code-search-file-buffer-name-prefix
                                  repo)
                                 (buffer-name))
            (kill-buffer buff))
        (kill-buffer buff)))))

(defvar github-code-search-minibuffer-timer nil
  "Timer for delaying minibuffer code search updates.")

(defun github-code-search-debounce--run-in-buffer (buffer timer-sym fn &rest
                                                          args)
  "Execute function FN with ARGS in BUFFER if it's live and visible.

Argument BUFFER is the buffer in which to run the function FN.

Argument TIMER-SYM is a symbol whose value is expected to be a timer object.

Argument FN is the function to be applied in the buffer.

Remaining arguments ARGS are the arguments to be passed to the function FN."
  (when (and buffer (buffer-live-p buffer))
    (let ((buff-wnd (get-buffer-window buffer)))
      (with-current-buffer buffer
        (if (and buff-wnd
                 (not (eq (selected-window)
                          buff-wnd)))
            (with-selected-window buff-wnd
              (apply fn args))
          (apply fn args))
        (when-let ((timer-value (symbol-value timer-sym)))
          (when (timerp timer-value)
            (cancel-timer timer-value)))))))

(defun github-code-search--debounce (timer-sym delay fn &rest args)
  "DELAY execution of FN with ARGS after delay, canceling previous timer.

Argument TIMER-SYM is a symbol whose value is the timer object to be potentially
canceled and reset.

Argument DELAY is a number representing the time, in seconds, to wait before
executing FN.

Argument FN is the function to be called after the delay.

Remaining arguments ARGS are passed to FN when it is called."
  (when-let ((timer-value (symbol-value timer-sym)))
    (when (timerp timer-value)
      (cancel-timer timer-value)))
  (set timer-sym (apply #'run-with-timer delay nil
                        #'github-code-search-debounce--run-in-buffer
                        (current-buffer)
                        timer-sym
                        fn
                        args)))

(defun github-code-search-find-other-file (repo)
  "Switch to a file from a GitHub repo.

Argument REPO is a string representing the name of the GitHub repository."
  (interactive (list (github-code-search-current-buffer-repo)))
  (let ((complete-fn (lambda (tree)
                       (let* ((alist   (mapcar
                                        (lambda (x)
                                          (cons (cdr (assq 'path x))
                                                (cdr (assq 'url x))))
                                        tree))
                              (path
                               (github-code-search-completing-read-with-keymap
                                repo
                                (mapcar
                                 #'car
                                 alist)
                                github-code-search-minibuffer-file-map
                                (lambda
                                  ()
                                  (add-hook
                                   'after-change-functions
                                   (lambda
                                     (&rest
                                      _)
                                     (github-code-search--debounce
                                      'github-code-search-minibuffer-timer
                                      1
                                      'github-code-search-minibuffer-preview-file))
                                   nil
                                   t))))
                              (url (cdr (assoc-string path
                                                      alist))))
                         (github-code-search-download-repo-path
                          repo
                          path
                          url)))))
    (if (github-code-search--paths-get repo)
        (funcall complete-fn (github-code-search--paths-get repo))
      (github-code-search-tree--fetch-repo-tree
       repo
       complete-fn))))

(defun github-code-search-clone-repo (repo)
  "Clone a GitHub repository.

Argument REPO is the repository to be cloned."
  (interactive (list (github-code-search-current-buffer-repo)))
  (github-code-search--gh-repo-action 'gh-repo-clone-repo repo))

;;;###autoload
(defun github-code-search-browse-repo (repo)
  "Open the current buffer's GitHub repository in a web browser.

Argument REPO is a string representing the GitHub repository to browse."
  (interactive (list (github-code-search-current-buffer-repo)))
  (browse-url (concat "https://github.com/" repo)))

(defun github-code-search-tree--current-repo-path ()
  "Return cons with repository name and file path."
  (cond ((derived-mode-p 'github-code-search-result-mode)
         (let ((prop (or
                      (when-let* ((val (get-text-property
                                        (point)
                                        'github-code-search-item))
                                  (item (alist-get 'repository
                                                   val)))
                        (string-join (delq nil
                                           (list (alist-get 'full_name
                                                            item)
                                                 (alist-get 'path
                                                            val)))
                                     "/"))
                      (get-text-property (point) 'button-data))))
           (when (stringp prop)
             (let* ((segments
                     (split-string (if (string-prefix-p
                                        "https://" prop)
                                       (url-filename
                                        (url-generic-parse-url
                                         prop))
                                     prop)
                                   "/" t))
                    (repo (string-join (seq-take segments 2) "/"))
                    (path (string-join (seq-drop segments 2) "/")))
               (cons repo path)))))
        (t
         (let* ((buff-name (buffer-name))
                (segments
                 (split-string (if (string-prefix-p
                                    github-code-search-file-buffer-name-prefix
                                    buff-name)
                                   (substring-no-properties
                                    buff-name
                                    (length
                                     github-code-search-file-buffer-name-prefix))
                                 buff-name)
                               "/" t))
                (repo (string-join (seq-take segments 2) "/"))
                (path (string-join (seq-drop segments 2) "/")))
           (cons repo path)))))

(defun github-code-search-download-repo-path (repo-name path url &optional
                                                        search-str)
  "Download and display a file from a GitHub repository.

Argument REPO-NAME is a string representing the name of the repository.

Argument PATH is a string representing the path within the repository.

Argument URL is a string representing the url to retrieve the file content.

Optional argument SEARCH-STR is a string used to highlight matches in the
retrieved file content."
  (let ((buff-name (concat github-code-search-file-buffer-name-prefix
                           repo-name "/"
                           path)))
    (if (get-buffer buff-name)
        (unless (get-buffer-window buff-name)
          (pop-to-buffer-same-window buff-name))
      (with-current-buffer (get-buffer-create buff-name)
        (setq header-line-format
              (list (concat (propertize " " 'display
                                        '(space :align-to 0))
                            (format "%s " (buffer-name)))
                    (propertize " Loading" 'face 'warning)))
        (pop-to-buffer-same-window buff-name))
      (url-retrieve url
                    (lambda (status &rest _)
                      (let ((buff (get-buffer buff-name)))
                        (if-let ((err
                                  (github-code-search--get-status-error
                                   status)))
                            (progn
                              (message err)
                              (when (buffer-live-p buff)
                                (with-current-buffer buff
                                  (setq header-line-format
                                        (list (car header-line-format)
                                              (propertize " Error " 'face 'error)
                                              err)))))
                          (let* ((data
                                  (when (and (boundp
                                              'url-http-end-of-headers)
                                             url-http-end-of-headers)
                                    (goto-char url-http-end-of-headers)
                                    (delete-region (point-min)
                                                   (point))
                                    (github-code-search-json-read-buffer
                                     'alist)))
                                 (code (decode-coding-string
                                        (base64-decode-string
                                         (alist-get
                                          'content
                                          data))
                                        'utf-8)))
                            (when (buffer-live-p buff)
                              (with-current-buffer buff
                                (erase-buffer)
                                (setq buffer-read-only nil)
                                (progn
                                  (save-excursion
                                    (insert code))
                                  (setq buffer-file-name
                                        (expand-file-name
                                         (substring-no-properties
                                          buff-name
                                          (length
                                           github-code-search-file-buffer-name-prefix))
                                         default-directory))
                                  (github-code-search--set-major-mode
                                   buffer-file-name))
                                (setq-local header-line-format
                                            (list " " 'header-line-indent
                                                  (buffer-name)
                                                  (propertize " Ready " 'face 'success)))
                                (unless (symbol-value
                                         'github-code-search-file-mode)
                                  (github-code-search-file-mode))
                                (setq buffer-undo-list nil)
                                (set-buffer-modified-p nil)
                                (goto-char (point-min))
                                (when search-str
                                  (github-code-search-higlight-matches
                                   search-str)
                                  (unless (get-buffer-window buff)
                                    (pop-to-buffer-same-window buff))
                                  (let ((wnd (get-buffer-window buff)))
                                    (with-selected-window wnd
                                      (when-let ((found (re-search-forward
                                                         (regexp-quote
                                                          search-str)
                                                         nil t 1)))
                                        (set-window-point wnd found)
                                        found))))))))))))))

(defun github-code-search-download-code (item &optional search-str)
  "Download and display GitHub code based on search results.

Argument ITEM is an association list containing information about a specific
ITEM from a GitHub code search.

Optional argument SEARCH-STR is a string specifying the text to search for
within the downloaded code."
  (github-code-search-download-repo-path
   (alist-get 'full_name
              (alist-get
               'repository item))
   (alist-get 'path item)
   (alist-get 'url item)
   search-str))

(defun github-code-search-higlight-matches (str)
  "Highlight matches of a given string in GitHub code search.

Argument STR is a string that represents the pattern to be highlighted in the
code."
  (let ((case-fold-search t)
        (re (regexp-quote str)))
    (save-excursion
      (goto-char (point-min))
      (with-silent-modifications (while (re-search-forward re nil t 1)
                                   (add-face-text-property
                                    (match-beginning 0)
                                    (match-end 0)
                                    'highlight))))))

(defun github-code-search-load-code-result-at-point ()
  "Download and display full file for the code result at the current point."
  (interactive)
  (let ((item (get-text-property (point) 'github-code-search-item))
        (str
         (get-text-property (point)
                            'github-code-search-str)))
    (github-code-search-window-with-other-window
     (github-code-search-download-code item
                                       str))))

(defun github-code-search--gh-repo-action (fn &rest args)
  "Execute FN with ARGS if gh-repo is available, else prompt to install.

Argument FN is a function to be called that is expected to be part of the
`gh-repo' package.

Remaining arguments ARGS are passed to the function FN when it is called."
  (if (progn
        (require 'gh-repo nil t)
        (fboundp fn))
      (apply fn args)
    (message
     "Install gh-repo from https://github.com/KarimAziev/gh-repo to use this action")))

(defun github-code-search-view-repo-files (repo)
  "Display repository files using `gh-repo-tree'.

Argument REPO is the name of the repository to view files from."
  (interactive (list (github-code-search-current-buffer-repo)))
  (github-code-search--gh-repo-action 'gh-repo-tree repo))

(transient-define-prefix github-code-search-file-result-menu ()
  "Menu with a list of github code search results commands."
  [:description (lambda ()
                  (if-let ((repo (github-code-search-current-buffer-repo)))
                      (format "Actions for repository %s" repo)
                    ""))
   [("f" "Find file" github-code-search-find-other-file
     :inapt-if-not github-code-search-current-buffer-repo)
    ("v" "View files" github-code-search-view-repo-files
     :inapt-if-not github-code-search-current-buffer-repo)
    ("c" "Clone"  github-code-search-clone-repo
     :inapt-if-not github-code-search-current-buffer-repo)
    ("r"
     github-code-search-browse-repo
     :description
     (lambda ()
       (concat "Browse "
               (if-let ((repo (github-code-search-current-buffer-repo)))
                   (concat "https://github.com/" repo)
                 "")))
     :inapt-if-not
     github-code-search-current-buffer-repo)]]
  [:if-derived github-code-search-result-mode
   [("+" "Load next page" github-code-search-next-page)
    ("e" "Toggle exact matches" github-code-search-toggle-exact)
    ("u" "Load next page" github-code-search-toggle-uniq)
    ("RET" "View this file" github-code-search-load-code-result-at-point)]])

;;;###autoload
(define-minor-mode github-code-search-file-mode
  "Minor mode for displaying remote github files."
  :lighter " github-code-search-file"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") #'github-code-search-find-other-file)
    (define-key map (kbd "C-c C-l") #'github-code-search-clone-repo)
    (define-key map (kbd "C-c C-d") #'github-code-search-view-repo-files)
    (define-key map (kbd "?") #'github-code-search-file-result-menu)
    (define-key map (kbd "f") #'github-code-search-view-repo-files)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "C-h ?") #'github-code-search-file-result-menu)
    map))

(defvar github-code-search-src-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")
                #'github-code-search-load-code-result-at-point)
    (define-key map (kbd "?") #'github-code-search-file-result-menu)
    (define-key map (kbd "f") #'github-code-search-find-other-file)
    (define-key map (kbd "C-c C-f") #'github-code-search-find-other-file)
    (define-key map (kbd "C-h ?") #'github-code-search-file-result-menu)
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
    (setq github-code-search-initial-value args)
    (github-code-search-request code query 1 exact
                                uniq)))

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
  (or
   (when (and (region-active-p)
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
                                                 array-type null-object
                                                 false-object)
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
  (setq github-code-search-initial-value (github-code-search-get-arguments))
  (funcall github-code-search-word-browse-fn
           (concat "https://gist.github.com/search?"
                   (github-code-search-get-browse-query))))

;;;###autoload
(defun github-code-search-browse ()
  "Search GitHub code via a web browser."
  (interactive)
  (setq github-code-search-initial-value (github-code-search-get-arguments))
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
                  :errorback (lambda (_err _headers status _req)
                               (let ((err
                                      (github-code-search--get-status-error
                                       status)))
                                 (when-let
                                     ((buff
                                       (get-buffer
                                        github-code-search-result-buffer-name)))
                                   (with-current-buffer buff
                                     (setq github-code-search-loading nil
                                           github-code-search-error
                                           (or err
                                               "An error occured")
                                           github-code-search-page
                                           (max 1
                                                (1- page)))
                                     (github-code-search-spinner--stop)
                                     (github-code-search-spinner-cleanup)
                                     (github-code-search-update-header-line)))
                                 (message (or err
                                              "An error occured"))))
                  :callback
                  (lambda (value _headers status _req)
                    (let ((buffer (get-buffer
                                   github-code-search-result-buffer-name)))
                      (when-let ((err
                                  (github-code-search--get-status-error
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
         (setq github-code-search-loading t)
         (github-code-search-request github-code-search--search-code
                                     github-code-search--search-extra-query
                                     (1+ github-code-search-page)
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

(defun github-code-search-spinner-overlay-make (start end &optional buffer
                                                      front-advance rear-advance
                                                      &rest props)
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
                                                   #'identity
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
        (run-with-timer 0.2 0.2 #'github-code-search-spinner-update (current-buffer))))

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
             (list github-code-search--search-code) 'symbols))
        (case-fold-search nil))
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
  (let ((filtered github-code-search-response)
        (hidden-count))
    (when github-code-search-uniq
      (setq filtered (github-code-search-filter-uniq filtered)))
    (when github-code-search-exact
      (setq filtered (github-code-search-filter-exact filtered)))
    (let ((inhibit-read-only t)
          (pos (point)))
      (erase-buffer)
      (setq hidden-count (- (length github-code-search-response)
                            (length filtered)))
      (github-code-search-insert-matches
       filtered
       github-code-search--search-code)
      (github-code-search-render-footer)
      (github-code-search-update-header-line hidden-count)
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

(defun github-code-search-request (code query page &optional exact uniq)
  "Search GitHub CODE and display results.

Argument CODE is the code snippet or keyword to search for.

Argument QUERY is the additional search parameters or filters.

Argument PAGE is the specific page number of the search results.

Optional argument EXACT is a boolean flag for exact match searches.

Optional argument UNIQ is a boolean flag for unique match searches."
  (let ((buffer (get-buffer-create
                 github-code-search-result-buffer-name)))
    (with-current-buffer buffer
      (let ((query-same
             (and (equal code github-code-search--search-code)
                  (equal query github-code-search--search-extra-query))))
        (unless (derived-mode-p 'github-code-search-result-mode)
          (github-code-search-result-mode))
        (unless (get-buffer-window buffer)
          (github-code-search-window-with-other-window
           (pop-to-buffer-same-window buffer)))
        (cond ((and
                query-same
                (not github-code-search-error)
                (= page github-code-search-page)
                (or (not (equal github-code-search-uniq uniq))
                    (not (equal github-code-search--search-extra-query
                                exact))))
               (setq github-code-search-exact exact)
               (setq github-code-search-uniq uniq)
               (github-code-search-revert))
              (t
               (unless query-same
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (setq github-code-search-response nil
                         github-code-search-total-count nil)))
               (setq github-code-search--search-code code
                     github-code-search--search-extra-query query
                     github-code-search-loading t
                     github-code-search-page page
                     github-code-search-error nil
                     github-code-search-exact exact
                     github-code-search-uniq uniq)
               (github-code-search-update-header-line)
               (github-code-search-spinner-start)
               (github-code-search-async-request
                code query github-code-search-page
                (lambda (value)
                  (let ((items (alist-get 'items value)))
                    (when (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (setq github-code-search-loading nil
                              github-code-search-error nil)
                        (setq github-code-search-total-count
                              (or
                               (alist-get 'total_count value)
                               github-code-search-total-count))
                        (setq buffer-read-only t)
                        (let ((inhibit-read-only t))
                          (when items
                            (setq github-code-search-response
                                  (if github-code-search-response
                                      (append github-code-search-response items)
                                    items)))
                          (github-code-search-revert)))))))))))))

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

(defvar-local github-code-search--old-header-line nil)

(defun github-code-search-update-header-line (&optional hidden-count)
  "Update header line with GitHub code search info.

Optional argument HIDDEN-COUNT is an integer representing the number of search
results that are not displayed."
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
                (cond (github-code-search-loading
                       (propertize " Loading" 'face 'warning))
                      ((stringp github-code-search-error)
                       (propertize
                        (truncate-string-to-width
                         github-code-search-error
                         70)
                        'face
                        'error))
                      (github-code-search-error
                       "Error")
                      (t (propertize " Ready" 'face 'success)))
                (string-join (delq nil
                                   (list
                                    (when github-code-search-uniq
                                      (propertize " [Uniq] " 'face 'warning))
                                    (when github-code-search-exact
                                      (propertize " [Exact] " 'face 'warning))
                                    (when (and hidden-count
                                               (> hidden-count 0))
                                      (format " %s results hidden" hidden-count))))
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
    (define-key map (kbd "v") #'github-code-search-view-repo-files)
    (define-key map (kbd "?") #'github-code-search-file-result-menu)
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

(defun github-code-search-json-read-buffer (&optional object-type array-type
                                                      null-object false-object)
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
                                     (require 'gh-repo nil t)
                                     (fboundp 'gh-repo-clone-repo))
                               (buttonize "Clone repo "
                                          #'gh-repo-clone-repo
                                          repo-name))
                             (when (progn
                                     (require 'gh-repo nil t)
                                     (fboundp 'gh-repo-tree))
                               (buttonize "View repo "
                                          #'gh-repo-tree
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

(defun github-code-search-languages-choices ()
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
                           github-code-search-langs-alist)))))

(defun github-code-search-queries-to-options (queries)
  "Convert GitHub code search QUERIES into command line options.

Argument QUERIES is a list."
  (nreverse
   (seq-reduce (lambda (acc item)
                 (if (stringp item)
                     (push item acc)
                   (pcase-let* ((`(,v . ,props) item)
                                (k (substring-no-properties v 0 1))
                                (pl (github-code-search--plist-remove
                                     '(:incompatible)
                                     props))
                                (vals (list (append (list (format "%s" k) v
                                                          (format "--%s=" v))
                                                    pl)
                                            (append (list
                                                     (format "%s" (upcase k))
                                                     (format "not %s" v)
                                                     (format "--not-%s=" v))
                                                    pl))))
                     (dolist (option vals)
                       (push option acc))))
                 acc)
               queries '())))

(defun github-code-search-make-incompatible-queries (queries)
  "Generate incompatible QUERIES for GitHub code search.

Argument QUERIES is a list of strings."
  (mapcar (pcase-lambda (`(,it . ,props))
            (let ((extra (plist-get props :incompatible)))
              (append (list (format "--%s=" it)
                            (format "--not-%s=" it))
                      (mapcar  (lambda (a)
                                 (let ((format-str
                                        (delq nil (list
                                                   (unless (string-prefix-p "--"
                                                                            a)
                                                     "--")
                                                   "%s"
                                                   (unless (string-suffix-p "="
                                                                            a)
                                                     "=")))))
                                   (format (string-join format-str) a)))
                               (pcase extra
                                 ((pred stringp)
                                  (list extra))
                                 (_ extra))))))
          (seq-filter #'consp queries)))

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

(transient-define-argument github-code-search-in-argument ()
  "Restrict search to the contents of the source code file, the file path, or both.
When you omit this qualifier, only the file contents are searched."
  :class 'transient-switches
  :description "in"
  :init-value (lambda (obj)
                (cond ((and (slot-boundp obj 'value)
                            (oref obj value))
                       (setf (slot-value obj 'value)
                             (oref obj value)))
                      (t (setf (slot-value obj 'value)
                               "--in=file"))))
  :argument-format "--in=%s"
  :argument-regexp "\\(?:file\\(?:,path\\)?\\|path\\)"
  :choices '("file" "path" "file,path"))

(transient-define-argument github-code-search-fork-argument ()
  "By default, forks are not shown in search results.
You can choose to include them in repository searches, and in code searches if
they meet certain criteria."
  :class 'transient-switches
  :description "fork:"
  :argument-format "--fork=%s"
  :argument-regexp "\\(?:only\\|true\\)"
  :choices '("true" "only"))

(defun github-code-search-make-toggled-description (var &optional description
                                                        align)
  "Toggle display of a variable's state with optional description.

Argument VAR is the variable to toggle.

Optional argument DESCRIPTION is the string to display; defaults to the string
representation of VAR.

Optional argument ALIGN is the column to align the toggle indicator; defaults to
32."
  (lambda ()
    (concat
     (propertize
      (or description (format "%s" var))
      'face
      (if
          (and (boundp var)
               (symbol-value var))
          'success nil))
     (propertize " " 'display
                 (list 'space :align-to (or align 32)))
     (if (and (boundp var)
              (symbol-value var))
         "[X]" "[ ]"))))

(defun github-code-search-describe-query ()
  "Display a description of the current GitHub code search query."
  (let
      ((result)
       (code (github-code-search-get-arg-value-from-args "code"))
       (filename-value (github-code-search-get-arg-value-from-args "filename")))
    (if (and (not code)
             (not filename-value))
        (propertize "Search for (filename or code required)" 'face
                    'transient-inapt-suffix)
      (pcase-dolist (`(,arg . ,_props)
                     (append (list (list "in")
                                   (list "fork"))
                             github-code-search-code-queries))
        (let ((value (and arg (github-code-search-get-arg-value-from-args arg)))
              (not-value (and arg
                              (github-code-search-get-arg-value-from-args
                               (concat
                                "not-"
                                arg)))))
          (when (or value not-value)
            (let* ((shortvalue (propertize
                                (format "'%s'"
                                        (truncate-string-to-width (or value
                                                                      not-value)
                                                                  20 nil
                                                                  nil
                                                                  t))
                                'face 'transient-value))
                   (parts
                    (delq nil
                          (pcase arg
                            ("in" (list "that appears in"
                                        (pcase value
                                          ((pred not) "the file contents")
                                          ("file" "the file contents")
                                          ("path" "the path")
                                          ((or "file,path" "path,file")
                                           "the file contents or the path"))))
                            ("extension" (list "with file extension" shortvalue))
                            ("path"
                             (list "within" shortvalue
                                   "directory, or in any of its subdirectories"))
                            ("language" (list "with language" shortvalue))
                            ((or "org" "user")
                             (list "owned by" shortvalue))
                            ("fork"
                             (pcase value
                               ("true" (list "in sources and forks"))
                               ("only" (list "only in forks"))))
                            (_ (list arg shortvalue))))))
              (when parts
                (push (if not-value
                          (concat "not " (string-join parts " "))
                        (string-join parts " "))
                      result))))))
      (let ((shortcode-val (and code
                                (propertize
                                 (format "'%s'"
                                         (truncate-string-to-width code 20
                                                                   nil
                                                                   nil
                                                                   t))
                                 'face 'transient-value)))
            (filename-formatted-value (and filename-value
                                           (propertize
                                            (format "'%s'"
                                                    (truncate-string-to-width
                                                     filename-value
                                                     20
                                                     nil
                                                     nil
                                                     t))
                                            'face
                                            'transient-value)))
            (prefix)
            (pad-str (make-string
                      (+ 2 (length
                            "RET"))
                      ?\s)))
        (setq prefix (string-join (cond ((and filename-formatted-value shortcode-val)
                                         (list "Search for" shortcode-val "in files named"
                                               filename-formatted-value))
                                        (filename-formatted-value
                                         (list "Search for files named" filename-formatted-value))
                                        (shortcode-val
                                         (list "Search for" shortcode-val
                                               (if (length> (split-string code nil t) 1)
                                                   "occurences"
                                                 "word"))))
                                  " "))
        (if result
            (concat prefix "\n"
                    (mapconcat
                     (lambda (it)
                       (concat pad-str "and " it))
                     (reverse result)
                     "\n"))
          prefix)))))

;;;###autoload (autoload 'github-code-search "github-code-search" nil t)
(transient-define-prefix github-code-search ()
  "A menu for GitHub code search with specific options."
  :incompatible (github-code-search-make-incompatible-queries
                 github-code-search-code-queries)
  :value (lambda ()
           github-code-search-initial-value)
  [:description github-code-search-query-description
   :setup-children
   (lambda (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       (oref transient--prefix command))
      (append (list
               (list "c" "code" "--code="
                     :class 'transient-option
                     :reader (lambda (&optional prompt initial-input history)
                               (read-string prompt (or
                                                    (thing-at-point 'symbol t)
                                                    initial-input)
                                            history))
                     :always-read nil
                     :init-value (lambda (obj)
                                   (when-let ((value
                                               (cond
                                                ((derived-mode-p
                                                  'github-code-search-result-mode)
                                                 github-code-search--search-code)
                                                ((and (region-active-p)
                                                      (use-region-p))
                                                 (string-trim
                                                  (buffer-substring-no-properties
                                                   (region-beginning)
                                                   (region-end))))
                                                ((oref obj value)
                                                 (oref obj value))
                                                (t
                                                 (when-let ((symb
                                                             (symbol-at-point)))
                                                   (symbol-name symb))))))
                                     (oset obj value value))))
               ""
               (list "i" 'github-code-search-in-argument)
               (list "r" "fork" 'github-code-search-fork-argument))
              (github-code-search-queries-to-options
               github-code-search-code-queries))))
   :class transient-column]
  ["Filtering"
   ("x" "Exact matches" "--exact" :if-not-derived github-code-search-result-mode)
   ("D" "Delete dublicates" "--uniq" :if-not-derived
    github-code-search-result-mode)
   ("x" github-code-search-toggle-exact
    :description ,(github-code-search-make-toggled-description
                   'github-code-search-exact
                   "Exact matches ")
    :if-derived
    github-code-search-result-mode
    :transient t)
   ("D" github-code-search-toggle-uniq
    :description
    ,(github-code-search-make-toggled-description
      'github-code-search-uniq
      "Delete dublicates")
    :if-derived
    github-code-search-result-mode
    :transient t)]
  ["Config"
   ("*" github-code-search-change-user
    :description  (lambda ()
                    (concat  "Github User: "
                             (if
                                 (or
                                  (not (car-safe
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
   ("g i" "Browse Gists" github-code-search-browse-gists)
   ("C-c C-a" "Copy query" (lambda ()
                             (interactive)
                             (let
                                 ((query (github-code-search-query-description)))
                               (kill-new query)
                               (message "Copied: %s" query)
                               query)))
   ("RET" github-code-search-code :description github-code-search-describe-query)]
  (interactive)
  (github-code-search-auth)
  (unless github-code-search-initial-value
    (setq github-code-search-initial-value
          (let* ((code (github-code-search-word-or-region))
                 (code-arg
                  (when (and code (not (string-empty-p code)))
                    (concat "--code=" code))))
            (delete nil
                    (list
                     code-arg
                     (if-let ((value
                               (github-code-search-get-default-language))
                              (l "--language="))
                         (concat l value)
                       (if-let ((ext
                                 (when buffer-file-name
                                   (file-name-extension
                                    buffer-file-name))))
                           (concat "--filename=" "*." ext)
                         (when (and buffer-file-name (not code-arg))
                           (concat "--filename="
                                   (file-name-nondirectory
                                    buffer-file-name))))))))))
  (transient-setup #'github-code-search))

(defun github-code-search--plist-remove (keys plist)
  "Remove KEYS and values from PLIST."
  (let* ((result (list 'head))
         (last result))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (new (and (not (memq key keys))
                       (list key val))))
        (when new
          (setcdr last new)
          (setq last (cdr new)))))
    (cdr result)))

(defun github-code-search--set-major-mode (filename)
  "Set buffer's major mode based on FILENAME and ensure font-lock.

Argument FILENAME is a string representing the name of the file for which to set
the major mode."
  (let ((buffer-file-name (or
                           (if (file-name-absolute-p filename)
                               filename
                             (expand-file-name filename default-directory)))))
    (ignore-errors
      (set-auto-mode)
      (font-lock-ensure))))

(defun github-code-search--get-status-error (status)
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

(defun github-code-search--json-reader (&optional object-type array-type
                                                  null-object false-object)
  "Parse JSON from GitHub response.

Optional argument OBJECT-TYPE specifies which Lisp type is used to represent
objects; it can be `hash-table', `alist' or `plist'. It defaults to `alist'.

Optional argument ARRAY-TYPE specifies which Lisp type is used to represent
arrays; it can be `list' or `vector'. It defaults to `vector'.

Optional argument NULL-OBJECT specifies which object to use to represent a JSON
null value. It defaults to `:null'.

Optional argument FALSE-OBJECT specifies which object to use to represent a JSON
false value. It defaults to `:false'."
  (let ((raw (ghub--decode-payload)))
    (and raw
         (condition-case nil
             (github-code-search-json-parse-string raw
                                                   object-type
                                                   array-type
                                                   null-object
                                                   false-object)
           ((json-parse-error json-readtable-error)
            `((message
               . ,(if (looking-at "<!DOCTYPE html>")
                      (if (re-search-forward
                           "<p>\\(?:<strong>\\)?\\([^<]+\\)"
                           nil t)
                          (match-string 1)
                        "error description missing")
                    (string-trim (buffer-substring
                                  (point)
                                  (point-max)))))
              (documentation_url . "https://github.com/magit/ghub/wiki/Github-Errors")))))))

(defun github-code-search--repo-get (resource &optional params &rest args)
  "Retrieve a GitHub repository's data using specified RESOURCE and parameters.

Argument RESOURCE: This is a required argument that specifies the RESOURCE to
get from the GitHub repository.

It should be a string.

Optional argument PARAMS: This argument is optional and can be used to pass
additional parameters to the `ghub-get' function.

It should be a list.

ARGS can be used to pass any number of additional arguments
to the `ghub-get' function."
  (let* ((auth (github-code-search-auth))
         (query (plist-get args :query))
         (query-str (mapconcat
                     (pcase-lambda (`(,k . ,v))
                       (format "%s=%s" k v))
                     (seq-filter #'cdr query)
                     "&"))
         (url
          (if (string-empty-p query-str)
              resource
            (concat (if (string-suffix-p "/" resource)
                        (substring-no-properties resource (1- (length
                                                               resource)))
                      resource)
                    "?" query-str))))
    (apply #'ghub-get url params
           :auth (cdr auth)
           :username (car auth)
           (github-code-search--plist-remove '(:query)
                                             args))))

(defun github-code-search--get-other-wind ()
  "Return another window or split sensibly if needed."
  (let ((wind-target
         (if (minibuffer-selected-window)
             (with-minibuffer-selected-window
               (let ((wind (selected-window)))
                 (or
                  (window-right wind)
                  (window-left wind)
                  (split-window-sensibly)
                  wind)))
           (let ((wind (selected-window)))
             (or
              (window-right wind)
              (window-left wind)
              (split-window-sensibly)
              wind)))))
    wind-target))

(defun github-code-search-tree--fetch-repo-tree (repo &optional callback
                                                      on-error)
  "Fetch GitHub REPO's file structure as a tree.

Argument REPO is a string representing the GitHub repository in the format
\"owner/repo\".

Optional argument CALLBACK is a function to be called with the result of the
fetch operation.

Optional argument ON-ERROR is a function to be called if an error occurs during
the fetch operation."
  (github-code-search--repo-get (format "repos/%s/git/trees/HEAD:?recursive=1"
                                        repo)
                                nil
                                :reader
                                (lambda (&rest _)
                                  (github-code-search--json-reader
                                   'alist
                                   'list))
                                :callback (lambda (response &rest _)
                                            (let* ((tree (cdr (assq 'tree
                                                                    response)))
                                                   (value (github-code-search--paths-put
                                                           repo
                                                           (seq-filter
                                                            (lambda (x)
                                                              (when (equal
                                                                     (cdr
                                                                      (assq
                                                                       'type
                                                                       x))
                                                                     "blob")
                                                                (cdr (assoc
                                                                      'path x))))
                                                            tree))))
                                              (if callback
                                                  (funcall callback value)
                                                tree)))
                                :errorback (lambda (_err _headers status _req)
                                             (if-let ((err
                                                       (github-code-search--get-status-error
                                                        status)))
                                                 (funcall
                                                  (or on-error #'message) err)))))

(provide 'github-code-search)
;;; github-code-search.el ends here
