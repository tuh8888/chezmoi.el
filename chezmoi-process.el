;;; chezmoi-process.el --- Persistent command process for chezmoi -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Optional command runner which executes chezmoi commands through one
;; PTY-backed shell.  This lets integrations which authorize a terminal
;; session, such as 1Password CLI, reuse that authorization.

;;; Code:

(require 'chezmoi-core)
(require 'easy-mmode)
(require 'subr-x)

(defcustom chezmoi-process-shell-command
  (or (executable-find "bash") "/bin/bash")
  "Bash executable used for the persistent chezmoi command process."
  :type 'file
  :group 'chezmoi)

(defvar chezmoi-process--process nil
  "Persistent shell process used for chezmoi commands.")

(defvar chezmoi-process--buffer nil
  "Private transport buffer for `chezmoi-process--process'.")

(defvar chezmoi-process--saved-runner nil
  "Command runner to restore when `chezmoi-process-mode' is disabled.")

(defun chezmoi-process--stop ()
  "Stop the persistent chezmoi shell and remove its transport buffer."
  (when (process-live-p chezmoi-process--process)
    (delete-process chezmoi-process--process))
  (setq chezmoi-process--process nil)
  (when (buffer-live-p chezmoi-process--buffer)
    (kill-buffer chezmoi-process--buffer))
  (setq chezmoi-process--buffer nil))

(defun chezmoi-process--start ()
  "Start and return the persistent shell used for chezmoi commands."
  (chezmoi-process--stop)
  (setq chezmoi-process--buffer (generate-new-buffer " *chezmoi-process*"))
  (let* ((ready-marker
          (format "__CHEZMOI_READY_%x%x__"
                  (emacs-pid) (random most-positive-fixnum)))
         (prompt-command
          (format "set +o history; unset HISTFILE; printf '%%s\\n' %s; unset PROMPT_COMMAND"
                  (shell-quote-argument ready-marker)))
         (process-environment
          (cons "NO_COLOR=1"
                (cons (concat "PROMPT_COMMAND=" prompt-command)
                      (cons "HISTFILE=/dev/null"
                            (cons "PS1=" process-environment))))))
    (setq chezmoi-process--process
          (make-process
           :name "chezmoi-process"
           :buffer chezmoi-process--buffer
           :command `(,chezmoi-process-shell-command
                      "--noprofile" "--norc" "-c"
                      ,(format "stty -echo; export PS1=; exec %s --noprofile --norc +o history -i"
                               (shell-quote-argument
                                chezmoi-process-shell-command)))
           :connection-type 'pty
           :noquery t))
    (while (and (process-live-p chezmoi-process--process)
                (with-current-buffer chezmoi-process--buffer
                  (goto-char (point-min))
                  (not (search-forward ready-marker nil t))))
      (accept-process-output chezmoi-process--process 0.1))
    (unless (process-live-p chezmoi-process--process)
      (error "Could not start chezmoi command process"))
    (with-current-buffer chezmoi-process--buffer
      (erase-buffer)))
  chezmoi-process--process)

(defun chezmoi-process--ensure ()
  "Return a live persistent shell process."
  (if (process-live-p chezmoi-process--process)
      chezmoi-process--process
    (chezmoi-process--start)))

(defun chezmoi-process-run-command (command)
  "Run shell COMMAND in the persistent chezmoi shell.
Return a `chezmoi--command-result' containing its status and output."
  (let* ((process (chezmoi-process--ensure))
         (token (format "%x%x" (emacs-pid) (random most-positive-fixnum)))
         (start-marker (format "__CHEZMOI_START_%s__" token))
         (end-marker (format "__CHEZMOI_END_%s__" token))
         (end-regexp (format "%s:\\([0-9]+\\)" (regexp-quote end-marker)))
         (command-with-directory
          (format "builtin cd -- %s && { %s; }"
                  (shell-quote-argument (expand-file-name default-directory))
                  command))
         status output completed)
    (unwind-protect
        (progn
          (with-current-buffer chezmoi-process--buffer
            (erase-buffer))
          (process-send-string
           process
           (format "printf '%%s\\n' %s; { %s; }; __chezmoi_status=$?; printf '\\n%%s:%%s\\n' %s \"$__chezmoi_status\"\n"
                   (shell-quote-argument start-marker)
                   command-with-directory
                   (shell-quote-argument end-marker)))
          (while (and (process-live-p process) (null status))
            (accept-process-output process 0.1)
            (with-current-buffer chezmoi-process--buffer
              (goto-char (point-min))
              (when (re-search-forward end-regexp nil t)
                (setq status (string-to-number (match-string 1)))
                (let ((end (match-beginning 0)))
                  (goto-char (point-min))
                  (unless (search-forward (concat start-marker "\n") nil t)
                    (error "Could not find chezmoi command start marker"))
                  (setq output
                        (buffer-substring-no-properties (point) (1- end)))))))
          (unless status
            (error "Chezmoi command process exited unexpectedly"))
          (setq completed t)
          (make-chezmoi--command-result :status status :output output))
      (unless completed
        (chezmoi-process--stop))
      (when (buffer-live-p chezmoi-process--buffer)
        (with-current-buffer chezmoi-process--buffer
          (erase-buffer))))))

;;;###autoload
(define-minor-mode chezmoi-process-mode
  "Use a persistent PTY-backed shell for noninteractive chezmoi commands."
  :global t
  :group 'chezmoi
  (if chezmoi-process-mode
      (progn
        (unless (eq chezmoi-command-runner #'chezmoi-process-run-command)
          (setq chezmoi-process--saved-runner chezmoi-command-runner))
        (setq chezmoi-command-runner #'chezmoi-process-run-command))
    (when (eq chezmoi-command-runner #'chezmoi-process-run-command)
      (setq chezmoi-command-runner
            (or chezmoi-process--saved-runner
                #'chezmoi--run-command-direct)))
    (setq chezmoi-process--saved-runner nil)
    (chezmoi-process--stop)))

(provide 'chezmoi-process)

;;; chezmoi-process.el ends here
