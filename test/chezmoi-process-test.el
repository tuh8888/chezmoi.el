;;; chezmoi-process-test.el --- Tests for chezmoi process handling -*- lexical-binding: t -*-

(require 'ert)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))
(require 'chezmoi-process nil t)
(require 'chezmoi-template)
(require 'chezmoi-dired)
(provide 'magit)
(require 'chezmoi)

(when (fboundp 'chezmoi-process-mode)
  (chezmoi-process-mode 1))

(defvar chezmoi-command)
(defvar chezmoi-command-error-regex)

(defun chezmoi-process-test--cleanup ()
  "Stop and remove the test command process."
  (when (fboundp 'chezmoi-process--stop)
    (chezmoi-process--stop)))

(ert-deftest chezmoi-process-mode-installs-and-restores-command-runner ()
  "The optional mode installs its PTY runner and restores the prior runner."
  (should (fboundp 'chezmoi-process-mode))
  (should (fboundp 'chezmoi-process-run-command))
  (let ((original-runner #'chezmoi--run-command-direct))
    (unwind-protect
        (progn
          (chezmoi-process-mode -1)
          (setq chezmoi-command-runner original-runner)
          (chezmoi-process-mode 1)
          (should (eq #'chezmoi-process-run-command chezmoi-command-runner))
          (chezmoi--run-command "printf started")
          (should (process-live-p chezmoi-process--process))
          (chezmoi-process-mode -1)
          (should-not (process-live-p chezmoi-process--process))
          (should (eq original-runner chezmoi-command-runner)))
      (setq chezmoi-command-runner original-runner)
      (when (fboundp 'chezmoi-process-mode)
        (chezmoi-process-mode 1)))))

(ert-deftest chezmoi-process-reuses-one-shell-session ()
  "Consecutive commands execute in the same shell session."
  (should (fboundp 'chezmoi--run-command))
  (unwind-protect
      (let ((first (chezmoi--run-command "printf '%s' \"$$\""))
            (second (chezmoi--run-command "printf '%s' \"$$\"")))
        (should (equal 0 (chezmoi--command-result-status first)))
        (should (equal 0 (chezmoi--command-result-status second)))
        (should-not (string-empty-p (chezmoi--command-result-output first)))
        (should (equal (chezmoi--command-result-output first)
                       (chezmoi--command-result-output second))))
    (chezmoi-process-test--cleanup)))

(ert-deftest chezmoi-process-isolates-command-results ()
  "A command result contains no output retained from earlier commands."
  (unwind-protect
      (progn
        (should
         (equal "first"
                (chezmoi--command-result-output
                 (chezmoi--run-command "printf first"))))
        (should
         (equal "second"
                (chezmoi--command-result-output
                 (chezmoi--run-command "printf second")))))
    (chezmoi-process-test--cleanup)))

(ert-deftest chezmoi-process-returns-nonzero-status-and-output ()
  "Failed commands return their status and captured diagnostic output."
  (unwind-protect
      (let ((result (chezmoi--run-command "printf failure; false")))
        (should (equal 1 (chezmoi--command-result-status result)))
        (should (equal "failure" (chezmoi--command-result-output result))))
    (chezmoi-process-test--cleanup)))

(ert-deftest chezmoi-process-disables-command-color-output ()
  "Commands inherit the standard environment request for plain output."
  (unwind-protect
      (let* ((process-environment
              (cl-remove-if (lambda (entry)
                              (string-prefix-p "NO_COLOR=" entry))
                            process-environment))
             (result (chezmoi--run-command "printf '%s' \"$NO_COLOR\"")))
        (should (equal "1" (chezmoi--command-result-output result))))
    (chezmoi-process-test--cleanup)))

(ert-deftest chezmoi-process-does-not-retain-command-history ()
  "The transport shell does not retain command text in Bash history."
  (unwind-protect
      (progn
        (chezmoi--run-command "printf history-sensitive-text")
        (let ((history
               (chezmoi--command-result-output
                (chezmoi--run-command "history"))))
          (should-not (string-match-p "history-sensitive-text" history))))
    (chezmoi-process-test--cleanup)))

(ert-deftest chezmoi-process-discards-session-after-interruption ()
  "Interrupting a command stops its session before another command runs."
  (let ((interrupt (run-at-time 0.05 nil (lambda () (signal 'quit nil)))))
    (unwind-protect
        (progn
          (should
           (eq :interrupted
               (condition-case nil
                   (chezmoi--run-command "sleep 10")
                 (quit :interrupted))))
          (should-not (process-live-p chezmoi-process--process))
          (let ((result (chezmoi--run-command "printf recovered")))
            (should (equal 0 (chezmoi--command-result-status result)))
            (should (equal "recovered"
                           (chezmoi--command-result-output result)))))
      (cancel-timer interrupt)
      (chezmoi-process-test--cleanup))))

(ert-deftest chezmoi-process-recovers-after-shell-exits ()
  "The next command starts a new session after the shell exits."
  (unwind-protect
      (let* ((first (chezmoi--run-command "printf '%s' \"$$\""))
             (_ (delete-process chezmoi-process--process))
             (second (chezmoi--run-command "printf '%s' \"$$\"")))
        (should (equal 0 (chezmoi--command-result-status second)))
        (should-not (equal (chezmoi--command-result-output first)
                           (chezmoi--command-result-output second))))
    (chezmoi-process-test--cleanup)))

(ert-deftest chezmoi-process-uses-each-commands-default-directory ()
  "Each command executes in the caller's current local directory."
  (let ((first-directory (make-temp-file "chezmoi-process-first-" t))
        (second-directory (make-temp-file "chezmoi-process-second-" t)))
    (unwind-protect
        (let ((first
               (let ((default-directory first-directory))
                 (chezmoi--run-command "pwd -P")))
              (second
               (let ((default-directory second-directory))
                 (chezmoi--run-command "pwd -P"))))
          (should (equal (concat (file-truename first-directory) "\n")
                         (chezmoi--command-result-output first)))
          (should (equal (concat (file-truename second-directory) "\n")
                         (chezmoi--command-result-output second))))
      (chezmoi-process-test--cleanup)
      (delete-directory first-directory)
      (delete-directory second-directory))))

(ert-deftest chezmoi-template-execute-reuses-command-session ()
  "Rendering templates repeatedly uses the persistent command session."
  (let ((script (make-temp-file "chezmoi-parent-pid-")))
    (unwind-protect
        (progn
          (write-region "#!/bin/sh\nprintf '%s' \"$PPID\"\n" nil script nil 'silent)
          (set-file-modes script #o700)
          (let ((chezmoi-command script)
                (chezmoi-command-error-regex "chezmoi:"))
            (let ((first (chezmoi-template-execute "first"))
                  (second (chezmoi-template-execute "second")))
              (should-not (string-empty-p first))
              (should (equal first second)))))
      (chezmoi-process-test--cleanup)
      (delete-file script))))

(ert-deftest chezmoi-dispatch-explicitly-disables-color ()
  "General chezmoi dispatches request output without ANSI color codes."
  (let ((script (make-temp-file "chezmoi-dispatch-color-")))
    (unwind-protect
        (progn
          (write-region
           (concat "#!/bin/sh\n"
                   "if [ \"$1\" = --color=false ]; then\n"
                   "  printf plain-path\n"
                   "else\n"
                   "  printf 'plain-path\\033[m'\n"
                   "fi\n")
           nil script nil 'silent)
          (set-file-modes script #o700)
          (let ((chezmoi-command script))
            (should (equal '("plain-path")
                           (chezmoi--dispatch "managed")))))
      (chezmoi-process-test--cleanup)
      (delete-file script))))

(ert-deftest chezmoi-dispatch-reuses-command-session ()
  "General chezmoi dispatches execute in the persistent command session."
  (let ((script (make-temp-file "chezmoi-dispatch-parent-pid-")))
    (unwind-protect
        (progn
          (write-region "#!/bin/sh\nprintf '%s\\n' \"$PPID\"\n" nil script nil 'silent)
          (set-file-modes script #o700)
          (let ((chezmoi-command script))
            (let ((first (car (chezmoi--dispatch "first")))
                  (second (car (chezmoi--dispatch "second"))))
              (should-not (string-empty-p first))
              (should (equal first second)))))
      (chezmoi-process-test--cleanup)
      (delete-file script))))

(ert-deftest chezmoi-dired-add-reuses-command-session ()
  "Adding several marked files uses the persistent command session."
  (let ((script (make-temp-file "chezmoi-dired-parent-pid-"))
        (log-file (make-temp-file "chezmoi-dired-parent-pids-")))
    (unwind-protect
        (progn
          (write-region
           "#!/bin/sh\nprintf '%s\\n' \"$PPID\" >> \"$CHEZMOI_TEST_LOG\"\n"
           nil script nil 'silent)
          (set-file-modes script #o700)
          (let ((chezmoi-command script)
                (process-environment
                 (cons (concat "CHEZMOI_TEST_LOG=" log-file)
                       process-environment)))
            (cl-letf (((symbol-function 'dired-get-marked-files)
                       (lambda () '("/tmp/first" "/tmp/second"))))
              (chezmoi-dired-add-marked-files))
            (let ((parent-pids
                   (split-string
                    (with-temp-buffer
                      (insert-file-contents log-file)
                      (buffer-string))
                    "\n" t)))
              (should (equal 2 (length parent-pids)))
              (should (equal (car parent-pids) (cadr parent-pids))))))
      (chezmoi-process-test--cleanup)
      (delete-file script)
      (delete-file log-file))))

;;; chezmoi-process-test.el ends here
