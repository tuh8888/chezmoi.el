;;; chezmoi-ediff.el --- Ediff integration for chezmoi -*- lexical-binding: t -*-

;; Author: Harrison Pielke-Lombardo
;; Maintainer: Harrison Pielke-Lombardo
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (age "0.1.4"))
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
(require 'chezmoi)

(defun chezmoi-age--config-assoc-recursive (alist &rest keys)
  "Recursively find KEYS in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

(defun chezmoi-age-get-recipients ()
  "Get age recipients."
  (let* ((config (chezmoi--get-config))
	 (recipient (gethash "recipient" (gethash "age" (chezmoi--get-config))))
	 (recipients (gethash "recipients" (gethash "age" (chezmoi--get-config)))))
    (append (if (string-empty-p recipient) nil (list recipient)) recipients)))

(defun chezmoi-age-get-identity ()
  "Get age recipients."
  (let ((config (chezmoi--get-config)))
    (gethash "identity" (gethash "age" (chezmoi--get-config)))))

(provide 'chezmoi-age)

;;; chezmoi-age.el ends here
