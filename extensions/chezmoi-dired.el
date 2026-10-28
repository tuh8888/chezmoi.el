;;; chezmoi-dired.el --- Dired integration for chezmoi -*- lexical-binding: t -*-

;; Author: Harrison Pielke-Lombardo
;; Maintainer: Harrison Pielke-Lombardo
;; Version: 1.1.1
;; Package-Requires: ((emacs "26.1") (chezmoi "1.1.0"))
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
(require 'chezmoi)

(defun chezmoi-dired-add-marked-files ()
  "Add files marked in Dired to source state."
  (interactive)
  (let ((failures nil))
    (dolist (file (dired-get-marked-files))
      (let ((result (chezmoi--run-command
                     (chezmoi--command-line
                      (concat "add " (shell-quote-argument file))))))
        (unless (eql 0 (chezmoi--command-result-status result))
          (push (cons file (string-trim
                            (chezmoi--command-result-output result)))
                failures))))
    (when failures
      (let ((b (get-buffer-create "*chezmoi-dired-add*")))
        (with-current-buffer b
          (erase-buffer)
          (dolist (failure (nreverse failures))
            (insert (format "%s:\n%s\n" (car failure) (cdr failure)))))
        (display-buffer b)
        (message "chezmoi add failed for %d file(s); see %s"
                 (length failures) (buffer-name b))))))

(defun chezmoi-dired-forget-marked-files ()
  "Remove files marked in Dired from source state."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (when (dired-mark-pop-up nil 'forget files #'yes-or-no-p
                             (format "Forget %d file(s) from source state? " (length files)))
      (dolist (file files)
        (shell-command (concat chezmoi-command " forget --force " (shell-quote-argument file)))))))

(provide 'chezmoi-dired)
;;; chezmoi-dired.el ends here
