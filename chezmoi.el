;;; chezmoi.el --- summary -*- lexical-binding: t -*-

;; Author: Harrison Pielke-Lombardo
;; Maintainer: Harrison Pielke-Lombardo
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3") magit)
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

;; commentary

;;; Code:

(require 'magit)

(defvar chezmoi|selected-file)

(defun chezmoi|diff ()
  "View output of =chezmoi diff= in a diff-buffer."
  (interactive)
  (let ((b (or (get-buffer "*chezmoi-diff*") (generate-new-buffer "*chezmoi-diff*"))))
    (with-current-buffer b
      (diff-mode)
      (whitespace-mode 0)
      (shell-command "chezmoi diff" b))))

(defun chezmoi|changed-p (f)
  "TODO."
  (> (length (shell-command-to-string (concat "chezmoi diff " f))) 0))

(defun shell-command-to-string-no-line (cmd)
  "TODO."
  (first (split-string (shell-command-to-string cmd) "\n")))

(defun chezmoi|ediff ()
  "Choose a target to merge with its source using ediff.
Note: Does not run =chezmoi merge=."
  (interactive)
  (let* ((managed-files (split-string (shell-command-to-string "chezmoi managed") "\n"))
         (changed-files (remove-if-not #'chezmoi|changed-p
                                       (remove-if #'file-directory-p  managed-files)))
         (selected-file (completing-read "Select a dotfile to merge:" changed-files))
         (source-file (shell-command-to-string-no-line (concat "chezmoi source-path " selected-file))))
    (ediff-files selected-file source-file)))

(defun chezmoi|magit-status ()
  "Show the status of the chezmoi source repository."
  (interactive)
  (magit-status (shell-command-to-string-no-line "chezmoi source-path")))

(defun chezmoi|find ()
  "Edit a source file managed by chezmoi.
Note: Does not run =chezmoi edit="
  (interactive)
  (let* ((managed-files (split-string (shell-command-to-string "chezmoi managed") "\n"))
         (changed-files (remove-if #'file-directory-p managed-files))
         (selected-file (completing-read "Select a dotfile to merge:" changed-files))
         (source-file (shell-command-to-string-no-line (concat "chezmoi source-path " selected-file))))
    (find-file source-file)
    (setq-local chezmoi|selected-file selected-file)))

(defun chezmoi|write ()
  "Run =chezmoi apply= on the target file associated with the buffer, overwriting the target with the source state."
  (interactive)
  (shell-command (concat "chezmoi apply " chezmoi|selected-file)))

(provide 'chezmoi)

;;; chezmoi.el ends here
