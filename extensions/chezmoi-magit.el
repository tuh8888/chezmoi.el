;;; chezmoi-magit.el --- Magit integration for chezmoi -*- lexical-binding: t -*-

;; Author: Harrison Pielke-Lombardo
;; Maintainer: Harrison Pielke-Lombardo
;; Version: 1.1.0
;; Package-Requires: ((emacs "26.1") (magit "3.0.0") (chezmoi "1.1.0"))
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

;; Provides `magit' integration for `chezmoi'.

;;; Code:

(require 'chezmoi)
(require 'magit)

;;;###autoload
(defun chezmoi-magit-status ()
  "Show the status of the chezmoi source repository."
  (interactive)
  (magit-status-setup-buffer (cl-first (chezmoi--dispatch "source-path"))))

(provide 'chezmoi-magit)

;;; chezmoi-magit.el ends here
