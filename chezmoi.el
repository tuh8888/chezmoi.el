;;; chezmoi.el --- summary -*- lexical-binding: t -*-

;; Author: Harrison Pielke-Lombardo
;; Maintainer: Harrison Pielke-Lombardo
;; Version: 1.0.0
;; Package-Requires: (dependencies)
;; Homepage: www.github.com/tuh8888/project-name
;; Keywords: keywords


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

(defun chezmoi|diff ()
  (interactive)
  (let ((b (or (get-buffer "*chezmoi-diff*") (generate-new-buffer "*chezmoi-diff*"))))
    (with-current-buffer b
      (diff-mode)
      (whitespace-mode 0)
      (shell-command "chezmoi diff" b))))

(-> "hello"
    (split-string "l")
    length)

(defun chezmoi|changed-p (f)
  (-> "chezmoi diff "
      (concat f)
      shell-command-to-string
      length
      (> 0)))

(defun shell-command-to-string-no-line (cmd)
  (-> cmd
      shell-command-to-string
      (split-string "\n")
      first))

(defun chezmoi|merge ()
  (interactive)
  (let* ((managed-files (-> "chezmoi managed"
                            shell-command-to-string
                            (split-string "\n")))
         (changed-files (->> managed-files
                             (remove-if #'file-directory-p)
                             (remove-if-not #'chezmoi|changed-p)))
         (selected-file (completing-read "Select a dotfile to merge:" changed-files))
         (source-file (-> "chezmoi source-path "
                          (concat selected-file)
                          shell-command-to-string-no-line)))
    (ediff-files selected-file source-file)))

(defun chezmoi|magit-status ()
  (interactive)
  (magit-status (shell-command-to-string-no-line "chezmoi source-path")))

(defun chezmoi|edit ()
  (interactive)
  (let* ((managed-files (-> "chezmoi managed"
                            shell-command-to-string
                            (split-string "\n")))
         (changed-files (->> managed-files
                             (remove-if #'file-directory-p)))
         (selected-file (completing-read "Select a dotfile to merge:" changed-files))
         (source-file (-> "chezmoi source-path "
                          (concat selected-file)
                          shell-command-to-string-no-line)))
    (find-file source-file)
    (setq-local chezmoi|selected-file selected-file)))

(defun chezmoi|apply ()
  (interactive)
  (-> "chezmoi apply "
      (concat chezmoi|selected-file)
      shell-command))

(provide 'chezmoi)

;;; chezmoi.el ends here
