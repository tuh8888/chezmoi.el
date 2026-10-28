;;; chezmoi-ediff.el --- Ediff integration for chezmoi -*- lexical-binding: t -*-

;; Author: Harrison Pielke-Lombardo
;; Maintainer: Harrison Pielke-Lombardo
;; Version: 1.1.0
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

;; Provides `ediff' integration for `chezmoi'.

;;; Code:

(require 'chezmoi)
(require 'ediff)

(defcustom chezmoi-ediff-force-overwrite t
  "Whether to force file overwrite when ediff finishes with identical buffers."
  :type '(boolean)
  :group 'chezmoi)

(defvar-local chezmoi-ediff--source-file nil
  "Current ediff source-file.")

(defun chezmoi-ediff--ediff-get-region-contents (n buf-type ctrl-buf &optional start end)
  "An overriding fn for `ediff-get-region-contents'.
Converts and applies template diffs from the source-file.
N, BUF-TYPE, CTRL-BUF, START, and END are all passed to `ediff'."
  (ediff-with-current-buffer
      (ediff-with-current-buffer ctrl-buf (ediff-get-buffer buf-type))
    (if (string-equal chezmoi-ediff--source-file (buffer-file-name))
        (chezmoi-template-execute (buffer-substring-no-properties
                                   (or start (ediff-get-diff-posn buf-type 'beg n ctrl-buf))
                                   (or end (ediff-get-diff-posn buf-type 'end n ctrl-buf))))
      (buffer-substring
       (or start (ediff-get-diff-posn buf-type 'beg n ctrl-buf))
       (or end (ediff-get-diff-posn buf-type 'end n ctrl-buf))))))

(defun chezmoi-ediff--ediff-cleanup-hook ()
  (when chezmoi-ediff-force-overwrite
    (when-let (source-file (or (with-current-buffer ediff-buffer-A chezmoi-ediff--source-file)
			       (with-current-buffer ediff-buffer-B chezmoi-ediff--source-file)))
      (when (equal (with-current-buffer ediff-buffer-A (buffer-string))
		   (with-current-buffer ediff-buffer-B (buffer-string)))
	(chezmoi-write source-file t)))))

(defvar chezmoi-ediff--ediff-quit-hook ()
  (advice-remove 'ediff-get-region-contents #'chezmoi-ediff--ediff-get-region-contents))

(defun chezmoi-ediff (file)
  "Choose a FILE to merge with its source using `ediff'.
Note: Does not run =chezmoi merge=."
  (interactive
   (list (chezmoi--completing-read "Select a dotfile to merge: "
				   (chezmoi-changed-files)
				   'project-file)))
  (let* ((ident (when (fboundp 'chezmoi-age-get-identity) (chezmoi-age-get-identity)))
	 (recips (when (fboundp 'chezmoi-age-get-recipients) (chezmoi-age-get-recipients)))
	 (age-always-use-default-keys (and (boundp 'age-default-identity)
					   (equal ident age-default-identity)
					   (equal (if (equal 1 (length recips))
						      (car recips)
						    recips)
						  age-default-recipient))))
    (let* ((source-file (chezmoi-find file)))
      (advice-add 'ediff-get-region-contents :override #'chezmoi-ediff--ediff-get-region-contents)
      (setq chezmoi-ediff--source-file source-file)
      (ediff-files source-file file)
      (add-hook 'ediff-cleanup-hook #'chezmoi-ediff--ediff-cleanup-hook nil t)
      (add-hook 'ediff-quit-hook #'chezmoi-ediff--ediff-quit-hook nil t))))

(provide 'chezmoi-ediff)
;;; chezmoi-ediff.el ends here
