;;; chezmoi-template.el --- Display chezmoi templates -*- lexical-binding: t -*-

;; Author: Harrison Pielke-Lombardo
;; Maintainer: Harrison Pielke-Lombardo
;; Version: 1.1.0
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

;; Chezmoi is a dotfile management system that uses a source-target state
;; architecture.  This package provides convenience functions for maintaining
;; synchronization between the source and target states when making changes to
;; your dotfiles through Emacs.  It provides alternatives to `find-file' and
;; `save-buffer' for source state files which maintain synchronization to the
;; target state.  It also provides diff/ediff tools for resolving when dotfiles
;; get out of sync.  Dired and magit integration is also provided.

;;; Code:
(require 'subr-x)
(require 'chezmoi-core)

(defcustom chezmoi-template-display-p nil
  "Whether to display templates."
  :type '(boolean)
  :group 'chezmoi
  :local t)

(defvar-local chezmoi-template--buffer-displayed-p nil
  "Whether all templates are currently displayed in buffer.")

(defface chezmoi-template-face '((t (:underline t :inherit font-lock-constant-face)))
  "Face for displaying chezmoi templates values."
  :group 'chezmoi)

(defcustom chezmoi-template-regex "{{ *\\(\\.[^[:space:]]* *\\)}}" ; (pcre-to-elisp "\\{\\{ \\.\\S+ \\}\\}")
  "Regex for detecting chezmoi templates."
  :type '(choice string regexp)
  :group 'chezmoi)

(defvar chezmoi-template-key-regex "\\."
  "Regex for splitting keys.")

(defun chezmoi-template-execute (template)
  "Convert a TEMPLATE string using chezmoi'."
  (thread-first "%s execute-template %s"
                (format chezmoi-command (shell-quote-argument template))
                shell-command-to-string))

(defun chezmoi-template--put-display-value (start end value &optional object)
  "Display the VALUE from START to END in string or buffer OBJECT."
  (unless (string-match-p chezmoi-command-error-regex value)
    (put-text-property start end 'display value object)
    (put-text-property start end 'chezmoi t object)
    (font-lock-flush start end)
    (font-lock-ensure start end)))

(defun chezmoi-template--remove-display-value (start end &optional object)
  "Remove displayed template from START to END in OBJECT.
VALUE is ignored."
  (when (and start end)
    (let ((value (get-text-property start 'display object)))
      (remove-text-properties start end `(
                                          display ,value
                                          chezmoi t)
                              object)
      (font-lock-ensure start end)
      (font-lock-flush start end))))

(defun chezmoi-template--funcall-over-matches (f buffer-or-name)
  "Call F on each matching template in BUFFER-OR-NAME.
F is called with the start of the match, the end of the match,
the template value and BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name
    (let ((match -1)
          ;; TODO Check if I need to account for the text properties updating this.
          (string (buffer-substring-no-properties (point-min) (point-max))))
      (while (setq match (string-match chezmoi-template-regex string (1+ match)))
        (let* ((start (match-beginning 0))
               (end (match-end 0))
               (template (substring string start end))
               (value (chezmoi-template-execute template)))
          (funcall f (1+ start) (1+ end) value buffer-or-name))))))

(defun chezmoi-template--funcall-over-display-properties (f start buffer-or-name)
  "Call F on each occurrence with display property in BUFFER-OR-NAME.
F is called with the start of the occurrence, the end of the occurrence,
the display property value, and BUFFER-OR-NAME.
When START is non-nil, find only the region around START."
  (with-current-buffer buffer-or-name
    (let ((end (or start 1))
          (buf (current-buffer)))
      (if start
          (let ((start (previous-single-property-change end 'chezmoi buf))
                (end (next-single-property-change start 'chezmoi buf)))
            (funcall f start end buffer-or-name))
        (while (and (setq start (next-single-property-change end 'chezmoi buf))
                    (setq end (next-single-property-change start 'chezmoi buf)))
          (funcall f start end buffer-or-name))))))

(defun chezmoi-template-buffer-display (&optional display-p start buffer-or-name)
  "Display templates found in BUFFER-OR-NAME.
If called interactively, toggle display of templates in current buffer.
Use DISPLAY-P to set display of templates on or off.
START is passed to `chezmoi-template--funcall-over-display-properties'."
  (interactive (list (let ((display-p (not chezmoi-template--buffer-displayed-p)))
                       (setq-local chezmoi-template-display-p display-p)
                       display-p)
                     nil))
  (remove-hook 'after-change-functions #'chezmoi-template--after-change 1)

  (let* ((buffer-or-name (or buffer-or-name (current-buffer)))
         (was-modified-p (buffer-modified-p buffer-or-name)))
    (setq chezmoi-template--buffer-displayed-p (and display-p chezmoi-template-display-p))
    (if chezmoi-template--buffer-displayed-p
        (when chezmoi-template-display-p
          (chezmoi-template--funcall-over-matches #'chezmoi-template--put-display-value buffer-or-name))
      (chezmoi-template--funcall-over-display-properties #'chezmoi-template--remove-display-value start buffer-or-name))
    (unless was-modified-p (with-current-buffer buffer-or-name
                             (set-buffer-modified-p nil))))

  (add-hook 'after-change-functions #'chezmoi-template--after-change nil 1))

(defun chezmoi-template--after-change (_ _ _)
  "Refresh templates after each change."
  (chezmoi-template-buffer-display nil)
  (chezmoi-template-buffer-display t))

(provide 'chezmoi-template)

;;; chezmoi-template.el ends here
