;;; chezmoi-dired.el --- Dired integration for chezmoi -*- lexical-binding: t -*-

;; Author: Harrison Pielke-Lombardo
;; Maintainer: Harrison Pielke-Lombardo
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
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

;; Provides `dired' integration for `chezmoi'.

;;; Code:

(require 'dired)

(defun chezmoi-dired-add-marked-files ()
  "Add files marked in Dired to source state."
  (interactive)
  (dolist (file (dired-get-marked-files))
    (shell-command (concat chezmoi-command " add " (shell-quote-argument file)))))

(provide 'chezmoi-dired)
;;; chezmoi-dired.el ends here
