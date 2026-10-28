;;; chezmoi-core-test.el --- Tests for chezmoi core command execution -*- lexical-binding: t -*-

(require 'ert)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))
(require 'chezmoi-core)

(ert-deftest chezmoi-core-uses-direct-command-runner-by-default ()
  "Core commands use the non-PTY runner unless an extension opts in."
  (should (boundp 'chezmoi-command-runner))
  (should (eq #'chezmoi--run-command-direct chezmoi-command-runner))
  (should-not (featurep 'chezmoi-process)))

(ert-deftest chezmoi-core-direct-runner-captures-status-and-output ()
  "The default runner captures combined command output and exit status."
  (let ((result
         (chezmoi--run-command
          "printf direct-output; printf direct-error >&2; exit 7")))
    (should (equal 7 (chezmoi--command-result-status result)))
    (should (equal "direct-outputdirect-error"
                   (chezmoi--command-result-output result)))))

;;; chezmoi-core-test.el ends here
