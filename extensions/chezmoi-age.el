;;; chezmoi-age.el --- Age encryption integration for chezmoi -*- lexical-binding: t -*-

;; Author: Harrison Pielke-Lombardo
;; Maintainer: Harrison Pielke-Lombardo
;; Version: 1.2.0
;; Package-Requires: ((emacs "26.1") (age "0.1.4") (chezmoi "1.2.0"))
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

;; Provides `age' integration for `chezmoi'.

;;; Code:
(require 'age)
(require 'chezmoi)

(defun chezmoi-age-get-recipients ()
  "Get age recipients."
  (let* ((config (chezmoi-get-config))
	 (recipient (gethash "recipient" (gethash "age" config)))
	 (recipients (gethash "recipients" (gethash "age" config))))
    (append (if (string-empty-p recipient) nil (list recipient)) recipients)))

(defun chezmoi-age-get-identity ()
  "Get age identity."
  (let ((config (chezmoi-get-config)))
    (gethash "identity" (gethash "age" config))))

(defun chezmoi-age--ediff-advice (orig-fn &rest args)
  "Call ORIG-FN with ARGS, reusing age keys when they match chezmoi's.
If chezmoi's configured identity and recipients are age's defaults, bind
`age-always-use-default-keys' so encrypted files open without prompting."
  (let* ((ident (chezmoi-age-get-identity))
	 (recips (chezmoi-age-get-recipients))
	 (age-always-use-default-keys (and (equal ident age-default-identity)
					   (equal (if (equal 1 (length recips))
						      (car recips)
						    recips)
						  age-default-recipient))))
    (apply orig-fn args)))

(advice-add 'chezmoi-ediff :around #'chezmoi-age--ediff-advice)

(provide 'chezmoi-age)

;;; chezmoi-age.el ends here
