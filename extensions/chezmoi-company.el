;;; chezmoi-company.el --- Company completion for chezmoi -*- lexical-binding: t -*-

;; Author: Harrison Pielke-Lombardo
;; Maintainer: Harrison Pielke-Lombardo
;; Version: 1.2.0
;; Package-Requires: ((emacs "26.1") (company "0.9.13") (chezmoi "1.2.0"))
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

;; Obsolete.  Company already reads `completion-at-point-functions' through
;; `company-capf', and `chezmoi-mode' adds `chezmoi-capf' there for you.
;; `chezmoi-company-backend' is kept as a thin wrapper so old configs work.

;;; Code:

(require 'chezmoi)
(require 'company)
(require 'company-capf)

(defun chezmoi-company-backend (command &optional arg &rest ignored)
  "Company backend for chezmoi, delegating to `chezmoi-capf'.
COMMAND, ARG, and IGNORED are passed to `company-capf'."
  (interactive (list 'interactive))
  (if (eq command 'interactive)
      (company-begin-backend 'chezmoi-company-backend)
    (let ((completion-at-point-functions '(chezmoi-capf)))
      (apply #'company-capf command arg ignored))))

(make-obsolete 'chezmoi-company-backend 'chezmoi-capf "1.2.0")

(provide 'chezmoi-company)

;;; chezmoi-company.el ends here
