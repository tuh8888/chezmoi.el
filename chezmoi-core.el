;;; chezmoi-core.el --- A package for interacting with chezmoi -*- lexical-binding: t -*-

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

(require 'cl-lib)

(defgroup chezmoi nil
  "Customization group for `chezmoi-mode'."
  :group 'chezmoi)

(defcustom chezmoi-command "chezmoi"
  "The location of the chezmoi command."
  :type '(string)
  :group 'chezmoi)

(defvar chezmoi-command-error-regex "chezmoi:"
  "Regex for detecting if chezmoi has encountered an error.")

(cl-defstruct chezmoi--command-result
  status
  output)

(defun chezmoi--command-line (args)
  "Build a noninteractive chezmoi command line with shell string ARGS."
  (format "%s %s" chezmoi-command args))

(defun chezmoi--run-command-direct (command)
  "Run shell COMMAND without a PTY and return its status and output."
  (with-temp-buffer
    (let* ((status (call-process shell-file-name nil (list t t) nil
                                 shell-command-switch command)))
      (make-chezmoi--command-result
       :status status
       :output (buffer-substring-no-properties (point-min) (point-max))))))

(defcustom chezmoi-command-runner #'chezmoi--run-command-direct
  "Function used to run noninteractive chezmoi command strings."
  :type 'function
  :group 'chezmoi)

(defun chezmoi--run-command (command)
  "Run shell COMMAND using `chezmoi-command-runner'."
  (funcall chezmoi-command-runner command))

(defvar chezmoi-source-state-prefix-attrs
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
  "Source state attribute prefixes.")

(defvar chezmoi-source-state-suffix-attrs
  '(".literal"
    ".tmpl")
  "Source state attribute suffixes.")

(provide 'chezmoi-core)

;;; chezmoi-core.el ends here
