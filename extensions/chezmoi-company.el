;;; chezmoi-company.el --- Company completion for chezmoi -*- lexical-binding: t -*-

;; Author: Harrison Pielke-Lombardo
;; Maintainer: Harrison Pielke-Lombardo
;; Version: 1.1.0
;; Package-Requires: ((emacs "27.1") (company "0.9.13") (chezmoi "1.1.0"))
;; Homepage: http://www.github.com/tuh8888/chezmoi.el
;; Keywords: vc


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Provides a `company' backend for `chezmoi'.

;;; Code:

(require 'chezmoi)
(require 'company)

(defun chezmoi-company--keys-at-point ()
  "Convert the point to a sequence of keys."
  (when-let ((thing (thing-at-point 'sexp t)))
    (split-string thing chezmoi-template-key-regex)))

(defun chezmoi-company--data-at-point ()
  "Chezmoi data corresponding to the key path at the current point."
  (let ((keys (remove "" (butlast (chezmoi-company--keys-at-point)))))
    (cl-reduce (lambda (data k) (gethash k data)) keys :initial-value (chezmoi-get-data))))

(defun chezmoi-company--prefix ()
  "Return prefix for company completion."
  (and (member 'chezmoi-mode local-minor-modes)
       (car (last (chezmoi-company--keys-at-point)))))

(defun chezmoi-company--candidates (prefix)
  "Return candidates for PREFIX for company completion.
Candidates are chezmoi data values corresponding to the path at point."
  (let* ((data (chezmoi-company--data-at-point))
         (candidates (if (hash-table-p data)
                         (hash-table-keys data)
                       (list data))))
    (cl-remove-if-not (lambda (c) (string-prefix-p prefix c)) candidates)))

(defun chezmoi-company--annotation (candidate)
  "Return annotation for CANDIDATE for company completion.
The value of the path if candidate is a string.  Otherwise indicate type."
  (let* ((data (chezmoi-company--data-at-point))
         (value (when (hash-table-p data) (gethash candidate data))))
    (cond ((stringp value) (format " (%s)" value))
          ((hash-table-p value) " <Object>")
          (t ""))))

(defun chezmoi-company-backend (command &optional arg &rest ignored)
  "Company backend for chezmoi.
Provides completion using =chezmoi data=.  COMMAND, ARG, and IGNORED
are passed to `company'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'chezmoi-company-backend))
    (prefix (chezmoi-company--prefix))
    (candidates (chezmoi-company--candidates arg))
    (annotation (chezmoi-company--annotation arg))))

(provide 'chezmoi-company)

;;; chezmoi-company.el ends here
