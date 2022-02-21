;;; chezmoi.el --- A package for interacting with chezmoi -*- lexical-binding: t -*-

;; Author: Harrison Pielke-Lombardo
;; Maintainer: Harrison Pielke-Lombardo
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (magit "3.0.0"))
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
;; architecture. This package provides convenience functions for maintaining
;; synchronization between the source and target states when making changes to
;; your dotfiles through Emacs. It provides alternatives to `find-file' and
;; `save-buffer' for source state files which maintain synchronization to the
;; target state. It also provides diff/ediff tools for resolving when dotfiles
;; get out of sync. Dired and magit integration is also provided.

;;; Code:

(require 'magit)
(require 'ediff)
(require 'cl-lib)

(defgroup chezmoi nil
  "Customization group for `chezmoi-mode'."
  :group 'chezmoi)

(defcustom chezmoi-command "chezmoi"
  "The location of the chezmoi command."
  :type '(string)
  :group 'chezmoi)

(defcustom chezmoi-display-templates t
  "Whether to display templates."
  :type '(boolean)
  :group 'chezmoi)

(defface chezmoi-template-face '((t (:underline t :inherit font-lock-constant-face)))
  "Face for displaying chezmoi templates values."
  :group 'chezmoi)

(defcustom chezmoi-template-regex ;; (pcre-to-elisp "\\{\\{ \\.\\S+ \\}\\}")
  "{{ \\.[^
    ]+ }}"
  "Regex for detecting chezmoi templates."
  :type '(choice string regexp)
  :group 'chezmoi)

(defcustom chezmoi-template-error-regex ;; (pcre-to-elisp "chezmoi: template:")
  "chezmoi: template:"
  "Regex for detecting if chezmoi has encountered an error."
  :type '(choice string regexp)
  :group 'chezmoi)

(defcustom chezmoi-source-state-prefix-attrs
  '("after_"
    "before_"
    "create_"
    "dot_"
    "empty_"
    "encrypted_"
    "exact_"
    "executable_"
    "literal_"
    "modify_"
    "once_"
    "onchange_"
    "private_"
    "readonly_"
    "remove_"
    "run_"
    "symlink_")
  "Source state attribute prefixes."
  :type '(list)
  :group 'chezmoi)

(defcustom chezmoi-source-state-suffix-attrs
  '(".literal"
    ".tmpl")
  "Source state attribute suffixes."
  :type '(list)
  :group 'chezmoi)

(defvar-local chezmoi--templates-displayed-p nil
  "Whether all templates are currently displayed.")

(defvar-local chezmoi--ediff-source-file nil
  "Current ediff source-file.")

(defun chezmoi--select-file (files prompt f)
  "Call F on the selected file from FILES, giving PROMPT."
  (funcall f (completing-read prompt files)))

(defun chezmoi--shell-files (cmd)
  "Helper function to parse files from CMD."
  (let ((result (shell-command-to-string cmd)))
    (split-string result "\n")))

(defun chezmoi-source-file (target-file)
  "Return the source file corresponding to TARGET-FILE."
  (let* ((cmd (concat chezmoi-command " source-path " (when target-file (shell-quote-argument target-file))))
         (files (chezmoi--shell-files cmd)))
    (cl-first files)))

(defun chezmoi-version ()
  "Get version number of chezmoi."
  (let* ((s (cl-first (chezmoi--shell-files (concat chezmoi-command " --version"))))
         (dev-re "\\(version \\(dev\\)\\)")
         (v-re " \\(v\\(\\([0-9]+\\.\\)?\\([0-9]+\\.\\)?\\(\\*\\|[0-9]+\\)\\)\\)")
         (re (concat dev-re "\\|" v-re)))
    (when (string-match re s)
      (or (match-string 4 s)
          (match-string 2 s)))))

(defun chezmoi--unchezmoi-source-file-name (source-file)
  "Remove chezmoi attributes from SOURCE-FILE."
  (let* ((base-name (file-name-base source-file))
         (ext (file-name-extension source-file))
         (base-name (if ext (file-name-with-extension base-name ext)
                      base-name))
         (base-name (cl-reduce (lambda (s attr) (string-replace attr "" s))
                               chezmoi-source-state-suffix-attrs
                               :initial-value base-name))
         (dir (file-name-directory source-file))
         (dir (when dir (cl-reduce (lambda (s attr) (let ((replacement (if (string= "dot_" attr) "." "")))
                                                 (string-replace attr replacement s)))
                                   chezmoi-source-state-prefix-attrs
                                   :initial-value dir)))
         (stop-parsing nil)
         attr)
    (while (and (not stop-parsing) (setq attr (cl-some (lambda (attr) (when (string-prefix-p attr base-name) attr)) chezmoi-source-state-prefix-attrs)))
      (when (string= "literal_" attr) (setq stop-parsing t))
      (setq base-name (substring base-name (length attr)))
      (when (string= "dot_" attr) (setq base-name (concat "." base-name))))

    (file-name-concat dir base-name)))

(defun chezmoi--manual-target-file (source-file)
  "Return the target file corresponding to SOURCE-FILE."
  (let* ((to-find (chezmoi--unchezmoi-source-file-name source-file))
         (potential-targets (cl-remove-if-not (lambda (f)
                                                (let* ((dir (string-replace "~" "~/.local/share/chezmoi" (file-name-directory f)))
                                                       (base (file-name-base f))
                                                       (ext (file-name-extension f))
                                                       (corrected-f (file-name-concat dir (if ext (file-name-with-extension base ext) base)))
                                                       (trying (expand-file-name corrected-f)))
                                                  (string= trying to-find)))
                                              (chezmoi-managed-files))))
    (cond ((zerop (length potential-targets)) (progn (message "No target found") nil))
          ((not (= 1 (length potential-targets)))
           (progn (message "Multiple targets found: %s. Using first" potential-targets)
                  (cl-first potential-targets)))
          (t (cl-first potential-targets)))))

(make-obsolete-variable 'chezmoi--manual-target-file 'chezmoi-target-file "0.0.1")

(defun chezmoi-target-file (source-file)
  "Return the target file corresponding to SOURCE-FILE."
  (let ((v (chezmoi-version)))
    (if (or (and v (string-match-p "^[0-9]" v) (version<= "2.12.0" v)) (string= "dev" v))
        (let* ((cmd (concat chezmoi-command " target-path " (when source-file (shell-quote-argument source-file))))
               (files (chezmoi--shell-files cmd)))
          (cl-first files))
      (chezmoi--manual-target-file source-file))))

(defun chezmoi-managed ()
  "List all files and directories managed by chezmoi."
  (let* ((cmd (concat chezmoi-command " managed"))
         (files (chezmoi--shell-files cmd)))
    (cl-map 'list (lambda (file) (concat "~/" file)) files)))

(defun chezmoi-managed-files ()
  "List only files managed by chezmoi."
  (cl-remove-if #'file-directory-p (chezmoi-managed)))

(defun chezmoi-write (&optional arg target-file)
  "Run =chezmoi apply= on the TARGET-FILE.
This overwrites the target with the source state.
With prefix ARG, use `shell' to run command."
  (interactive "P")
  (let* ((f (if target-file target-file (chezmoi-target-file (buffer-file-name))))
         (cmd (concat chezmoi-command " apply " (shell-quote-argument f))))
    (if (not arg)
        (if (= 0 (shell-command cmd))
            (message "Wrote %s" f)
          (message "Failed to write file. Use chezmoi-write with prefix arg to resolve with chezmoi."))
      (shell "*Chezmoi Shell*")
      (insert cmd)
      (comint-send-input))))

(defun chezmoi-diff (arg)
  "View output of =chezmoi diff= in a diff-buffer.
If ARG is non-nil, switch to the diff-buffer."
  (interactive "i")
  (let ((b (get-buffer-create "*chezmoi-diff*")))
    (with-current-buffer b
      (erase-buffer)
      (shell-command (concat chezmoi-command " diff") b))
    (unless arg
      (switch-to-buffer b)
      (diff-mode)
      (whitespace-mode 0))
    b))

(defun chezmoi-changed-files ()
  "Use chezmoi diff to return the files that have changed."
  (let ((line-beg nil))
    (with-current-buffer (chezmoi-diff t)
      (goto-char (point-max))
      (let ((files nil))
        (while (setq line-beg (re-search-backward "^\\+\\{3\\} .*" nil t))
          (let ((file-name (substring
                            (buffer-substring-no-properties line-beg
                                                            (line-end-position))
                            5)))
            (push (concat "~" file-name) files)))
        files))))

(defun chezmoi-find (file)
  "Edit a source FILE managed by chezmoi.
If the target file has the same state as the source file,add a hook to
`save-buffer' that applies the source state to the target state. This way, when
the buffer editing the source state is saved the target state is kept in sync.
Note: Does not run =chezmoi edit=."
  (interactive
   (list (completing-read
          "Select a dotfile to edit: "
          (chezmoi-managed-files)
          nil t)))
  (let ((source-file (chezmoi-source-file file)))
    (find-file source-file)
    (let ((mode (assoc-default
                 (file-name-nondirectory file)
                 auto-mode-alist
                 'string-match)))
      (when mode (funcall mode)))
    (message file)
    (chezmoi-mode)
    source-file))

(defun chezmoi-convert-template (template)
  "Convert a TEMPLATE string using chezmoi'."
  (let* ((cmd (concat chezmoi-command " execute-template " (shell-quote-argument template))))
    (shell-command-to-string cmd)))

(defun chezmoi--ediff-get-region-contents (n buf-type ctrl-buf &optional start end)
  "An overriding fn for `ediff-get-region-contents'.
Converts and applies template diffs from the source-file."
  (ediff-with-current-buffer
      (ediff-with-current-buffer ctrl-buf (ediff-get-buffer buf-type))
    (if (string-equal chezmoi--ediff-source-file (buffer-file-name))
        (chezmoi-convert-template (buffer-substring-no-properties
                                   (or start (ediff-get-diff-posn buf-type 'beg n ctrl-buf))
                                   (or end (ediff-get-diff-posn buf-type 'end n ctrl-buf))))
      (buffer-substring
       (or start (ediff-get-diff-posn buf-type 'beg n ctrl-buf))
       (or end (ediff-get-diff-posn buf-type 'end n ctrl-buf))))))

(defun chezmoi--remove-ediff-get-region-contents ()
  "Remove advice overriding `ediff-get-region-contents'."
  (advice-remove 'ediff-get-region-contents #'chezmoi--ediff-get-region-contents))

(defun chezmoi-ediff (dotfile)
  "Choose a DOTFILE to merge with its source using `ediff'.
Note: Does not run =chezmoi merge=."
  (interactive
   (list (completing-read
          "Select a dotfile to merge: "
          (chezmoi-changed-files)
          nil t)))
  (let ((source-file (chezmoi-find dotfile)))
    (advice-add 'ediff-get-region-contents :override #'chezmoi--ediff-get-region-contents)
    (setq chezmoi--ediff-source-file source-file)
    (ediff-files source-file dotfile)
    (add-hook 'ediff-quit-hook #'chezmoi--remove-ediff-get-region-contents nil t)))

(defun chezmoi-magit-status ()
  "Show the status of the chezmoi source repository."
  (interactive)
  (magit-status-setup-buffer (chezmoi-source-file nil)))

(defun chezmoi--select-files (files prompt f)
  "Iteratively select file from FILES given PROMPT and apply F to each selected.
Selected files are removed after they are selected."
  (let ((files (cl-copy-list files)))
    (while files
      (chezmoi--select-file files
                            (concat prompt " (C-g to stop): ")
                            (lambda (file)
                              (funcall f file)
                              (setq files (remove file files)))))))

(defun chezmoi-write-from-target (target-file)
  "Apply the TARGET-FILE's state to the source file buffer.
Useful for files which are autogenerated outside of chezmoi."
  (interactive (list (chezmoi-target-file (buffer-file-name))))
  (with-current-buffer (find-file-noselect (chezmoi-source-file target-file))
    (replace-buffer-contents (find-file-noselect target-file))))

(defun chezmoi-write-files ()
  "Force overwrite multiple dotfiles with their source state."
  (interactive)
  (chezmoi--select-files (chezmoi-changed-files)
                         "Select dotfile to apply source state changes"
                         #'(lambda (f) (chezmoi-write nil f))))

(defun chezmoi-write-files-from-target ()
  "Force overwrite the source state of multiple dotfiles with their target state."
  (interactive)
  (chezmoi--select-files (chezmoi-changed-files)
                         "Select a dotfile to overwrite its source state with target state"
                         #'chezmoi-write-from-target))

(defun chezmoi-open-other ()
  "Open buffer's target file."
  (interactive)
  (let* ((f (buffer-file-name))
         (other (if (member f (cl-mapcar #'expand-file-name (chezmoi-managed-files)))
                    (chezmoi-source-file f)
                  (chezmoi-target-file f))))
    (find-file other)))

(defun chezmoi-dired-add-marked-files ()
  "Add files marked in Dired to source state."
  (interactive)
  (dolist (file (dired-get-marked-files))
    (shell-command (concat chezmoi-command " add " (shell-quote-argument file)))))

(defun chezmoi--write-hook ()
  "Hook for writing the source-file."
  (chezmoi-write nil))

(defun chezmoi--put-display-value (start end value &optional object)
  "Display the VALUE from START to END in string or buffer OBJECT."
  (unless (string-match-p chezmoi-template-error-regex value)
    (put-text-property start end 'display value object)
    (put-text-property start end 'chezmoi t object)
    (font-lock-flush start end)
    (font-lock-ensure start end)))

(defun chezmoi--remove-display-value (start end &optional object)
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

(defun chezmoi--funcall-over-matches (f buffer-or-name)
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
               (value (chezmoi-convert-template template)))
          (funcall f (1+ start) (1+ end) value buffer-or-name))))))

(defun chezmoi--funcall-over-display-properties (f start buffer-or-name)
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
            (message "%s %s" start end)
            (funcall f start end buffer-or-name))
        (while (and (setq start (next-single-property-change end 'chezmoi buf))
                    (setq end (next-single-property-change start 'chezmoi buf)))
          (funcall f start end buffer-or-name))))))

(defun chezmoi-display-buffer-templates (&optional display-p start buffer-or-name)
  "Display templates found in BUFFER-OR-NAME.
If called interactively, toggle display of templates in current buffer.
Use DISPLAY-P to set display of templates on or off.
START is passed to `chezmoi--funcall-over-display-properties'."
  (interactive (list (let ((display-p (not chezmoi--templates-displayed-p)))
                       (setq-local chezmoi-display-templates display-p)
                       display-p)
                     nil))
  (remove-hook 'after-change-functions #'chezmoi--after-change 1)

  (let* ((buffer-or-name (or buffer-or-name (current-buffer)))
         (was-modified-p (buffer-modified-p buffer-or-name)))
    (setq chezmoi--templates-displayed-p (and display-p chezmoi-display-templates))
    (if chezmoi--templates-displayed-p
        (when chezmoi-display-templates
          (chezmoi--funcall-over-matches #'chezmoi--put-display-value buffer-or-name))
      (chezmoi--funcall-over-display-properties #'chezmoi--remove-display-value start buffer-or-name))
    (unless was-modified-p (with-current-buffer buffer-or-name
                             (set-buffer-modified-p nil))))

  (add-hook 'after-change-functions #'chezmoi--after-change nil 1))

(defun chezmoi--after-change (_ _ _)
  "Refresh templates after each change."
  (chezmoi-display-buffer-templates nil)
  (chezmoi-display-buffer-templates t))

(defun chezmoi-font-lock-keywords ()
  "Keywords for font lock."
  `((,chezmoi-template-regex 0 'chezmoi-template-face prepend)))

(defun chezmoi-get-data ()
  ""
  (json-parse-string (shell-command-to-string (concat "chezmoi " "data"))))

(define-minor-mode chezmoi-mode
  "Chezmoi mode for source files."
  :group 'chezmoi
  (if chezmoi-mode
      (progn
        (unless (member (chezmoi-target-file (buffer-file-name)) (chezmoi-changed-files))
          (add-hook 'after-save-hook #'chezmoi--write-hook 0 t))
        (add-hook 'after-change-functions #'chezmoi--after-change nil 1)

        (font-lock-add-keywords nil (chezmoi-font-lock-keywords) 'append)
        (chezmoi-display-buffer-templates t)
        (font-lock-ensure (point-min) (point-max)))
    (progn
      (chezmoi-display-buffer-templates nil)

      (remove-hook 'after-save-hook #'chezmoi--write-hook t)
      (remove-hook 'after-change-functions #'chezmoi--after-change t)

      (font-lock-remove-keywords nil (chezmoi-font-lock-keywords))
      (font-lock-ensure (point-min) (point-max)))))

(provide 'chezmoi)

;;; chezmoi.el ends here
