;;; github-code-search.el --- Configure code search -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/github-code-search
;; Keywords: tools
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1"))

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

(defcustom github-code-search-word-browse-fn 'browse-url
  "Function to browse results of code search."
  :type 'function
  :group 'github-code-search)

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
     "Emacs Lisp")
    ((or 'python-ts-mode 'python-mode)
     "Python")
    ((or 'cmake-ts-mode 'cmake-mode)
     "CMake")
    ((or 'c-or-c++-ts-mode
         'c-ts-base-mode
         'c-mode)
     "C")
    ((or 'yaml-ts-mode 'yaml-mode)
     "YAML")
    ((or 'html-ts-mode 'html-mode)
     "HTML")
    ((or 'css-ts-mode 'css-ts-mode)
     "CSS")
    ((or 'typescript-ts-mode
         'typescript-ts-base-mode
         'typescript-mode)
     "Typescript")
    ((or 'bash-ts-mode 'sh-mode)
     "Shell")
    ((or 'java-ts-mode)
     "Java")
    ((or 'js-ts-mode
         'js-mode 'js-base-mode 'js2-mode)
     "Javascript")
    ((or 'json-ts-mode 'json-mode)
     "JSON")
    ((or 'clojurescript-mode 'clojure-mode)
     "Clojure")
    ('org-mode
     "Org")))

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
(defun github-code-search-do ()
  "Search from transient."
  (interactive)
  (let* ((args (transient-args transient-current-command))
         (filename (transient-arg-value "--path=" args))
         (code (transient-arg-value "--code=" args))
         (language (car (last (cdr (assoc (transient-arg-value "--language="
                                                               args)
                                          github-code-search-langs-alist)))))
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

;;;###autoload (autoload 'github-code-search "github-code-search" nil t)
(transient-define-prefix github-code-search ()
  "Command dispatcher."
  :value
  (lambda ()
    (let* ((code (github-code-search-word-or-region))
           (code-arg (when (and code (not (string-empty-p code)))
                       (concat "--code=" code))))
      (delete nil
              (list
               (when-let ((value (github-code-search-get-default-language))
                          (l "--language="))
                 (concat l value))
               code-arg
               (if-let ((ext (when buffer-file-name
                               (file-name-extension buffer-file-name))))
                   (concat "--path=" "*." ext)
                 (when (and buffer-file-name (not code-arg))
                   (concat "--path="
                           (file-name-nondirectory buffer-file-name))))))))
  ["Arguments"
   ("c" "code" "--code="
    :class transient-option
    :always-read t)
   ("l" "language" "--language="
    :choices
    (lambda (str pred action)
      (let ((alist (copy-tree
                    github-code-search-langs-alist)))
        (if (eq action 'metadata)
            `(metadata
              (annotation-function .
                                   github-code-search-annotate-language))
          (complete-with-action action alist
                                str
                                pred)))))
   ("p" "path" "--path=" :class transient-option)
   ("u" "user" "--user=" :class transient-option)]
  ["Exclude"
   ("U" "not-user" "--not-user=" :class transient-option)]
  ["Actions"
   ("RET" "Run" github-code-search-do)]
  (interactive)
  (setq github-code-search-langs-alist (or github-code-search-langs-alist
                                           (github-code-search-init-languages)))
  (transient-setup 'github-code-search))

(provide 'github-code-search)
;;; github-code-search.el ends here