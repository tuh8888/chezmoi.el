;;; chezmoi-ediff.el --- Ediff integration for chezmoi -*- lexical-binding: t -*-

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

;; Provides `ediff' integration for `chezmoi'.

;;; Code:

(require 'chezmoi)
(require 'ediff)
(require 'chezmoi-template)
(require 'json)

(defun chezmoi--get-config()
  (with-temp-buffer
    (let* ((chez (make-process :name "chezmoi-dump-config"
                              :buffer (current-buffer)
                              :stderr nil
                              :command (list "chezmoi" "dump-config")
                              :sentinel 'ignore
                              :connection-type 'pipe))
           (json-array-type 'list))
      (process-send-eof chez)
      (while (accept-process-output chez))
      (if (eq 0 (process-exit-status chez))
          (progn
            (goto-char (point-min))
            (json-read))
        (message "error decoding: %s" (buffer-string))))))

(defun chezmoi--config-assoc-recursive (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

(defun chezmoi-get-recipients()
  (let* ((config (chezmoi--get-config))
         (recipient (chezmoi--config-assoc-recursive config 'age 'recipient))
         (recipients (chezmoi--config-assoc-recursive config 'age 'recipients)))
    (append (if (string-empty-p recipient) nil recipient) recipients)))

(defvar-local chezmoi-ediff--source-file nil
  "Current ediff source-file.")

(defun chezmoi-ediff--ediff-get-region-contents (n buf-type ctrl-buf &optional start end)
  "An overriding fn for `ediff-get-region-contents'.
Converts and applies template diffs from the source-file."
  (ediff-with-current-buffer
      (ediff-with-current-buffer ctrl-buf (ediff-get-buffer buf-type))
    (if (string-equal chezmoi-ediff--source-file (buffer-file-name))
        (chezmoi-template-execute (buffer-substring-no-properties
                                   (or start (ediff-get-diff-posn buf-type 'beg n ctrl-buf))
                                   (or end (ediff-get-diff-posn buf-type 'end n ctrl-buf))))
      (buffer-substring
       (or start (ediff-get-diff-posn buf-type 'beg n ctrl-buf))
       (or end (ediff-get-diff-posn buf-type 'end n ctrl-buf))))))

(defun chezmoi-ediff (dotfile)
  "Choose a DOTFILE to merge with its source using `ediff'.
Note: Does not run =chezmoi merge=."
  (interactive
   (list (chezmoi--completing-read "Select a dotfile to merge: "
				   (chezmoi-changed-files)
				   'project-file)))
  (let* ((source-path (chezmoi-source-file dotfile))
         (sourcebuffer (create-file-buffer source-path))
         (source-file (chezmoi-find dotfile))
         (file-a (find-file-noselect dotfile)))
    (advice-add 'ediff-get-region-contents :override #'chezmoi-ediff--ediff-get-region-contents)
    (setq chezmoi-ediff--source-file source-file)
    (with-current-buffer sourcebuffer
      (setq-local age-file-recipients (chezmoi-get-recipients))
      (insert-file-contents source-file t))
    (ediff-buffers file-a sourcebuffer)
    (add-hook 'ediff-quit-hook
              #'(lambda () (advice-remove 'ediff-get-region-contents #'chezmoi-ediff--ediff-get-region-contents))
              nil t)))

(provide 'chezmoi-ediff)
;;; chezmoi-ediff.el ends here
