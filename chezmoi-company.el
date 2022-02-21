;;; chezmoi-company.el --- Company completion for chezmoi -*- lexical-binding: t -*-

;; Author: Harrison Pielke-Lombardo
;; Maintainer: Harrison Pielke-Lombardo
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (magit "3.0.0"))
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

;; Provides a `company' backend `chezmoi'.

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'chezmoi)

(defun chezmoi-company--key-path-string-to-keys (string)
  ""
  (split-string string "\\."))

(defun chezmoi-company--get-in-data (data keys)
  ""
  (cl-reduce (lambda (data k) (gethash k data)) keys :initial-value data))

(defun chezmoi-company--keys-at-point ()
  ""
  (when-let ((thing (thing-at-point 'sexp t)))
    (chezmoi-company--key-path-string-to-keys thing)))

(defun chezmoi-company--data-at-point ()
  ""
  (let ((keys (remove "" (butlast (chezmoi-company--keys-at-point)))))
    (chezmoi-company--get-in-data (chezmoi-get-data) keys)))

(defun chezmoi-company--candidates (prefix)
  ""
  (let* ((data (chezmoi-company--data-at-point))
         (candidates (if (hash-table-p data)
                         (hash-table-keys data)
                       (list data))))
    (cl-remove-if-not (lambda (c) (string-prefix-p prefix c)) candidates)))

(defun chezmoi-company--annotation (candidate)
  ""
  (let* ((data (chezmoi-company--data-at-point))
         (value (when (hash-table-p data) (gethash candidate data))))
    (if (stringp value)
        (format " (%s)" value)
      "")))

(defun chezmoi-company-backend (command &optional arg &rest ignored)
  ""
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'chezmoi-company-backend))
    (prefix (and (member 'chezmoi-mode local-minor-modes)
                 (car (last (chezmoi-company--keys-at-point)))))
    (candidates (chezmoi-company--candidates arg))
    (annotation (chezmoi-company--annotation arg))))
