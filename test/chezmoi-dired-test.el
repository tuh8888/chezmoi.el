;;; chezmoi-dired-test.el --- Tests for chezmoi Dired integration -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))
(add-to-list 'load-path
             (expand-file-name
              "../extensions"
              (file-name-directory (or load-file-name buffer-file-name))))
(require 'chezmoi-dired)

(ert-deftest chezmoi-dired-add-reports-failures ()
  "Failed adds are collected into a buffer and reported."
  (let ((chezmoi-command-runner
         (lambda (command)
           (if (string-match-p "bad" command)
               (make-chezmoi--command-result
                :status 1 :output "chezmoi: bad: no such file\n")
             (make-chezmoi--command-result :status 0 :output ""))))
        (messages nil))
    (cl-letf (((symbol-function 'dired-get-marked-files)
               (lambda () '("/tmp/good" "/tmp/bad")))
              ((symbol-function 'display-buffer) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (chezmoi-dired-add-marked-files))
    (should (string-match-p "failed for 1 file" (car messages)))
    (with-current-buffer "*chezmoi-dired-add*"
      (should (equal "/tmp/bad:\nchezmoi: bad: no such file\n"
                     (buffer-string))))))

(ert-deftest chezmoi-dired-add-is-quiet-on-success ()
  "Successful adds do not report anything."
  (let ((chezmoi-command-runner
         (lambda (_command)
           (make-chezmoi--command-result :status 0 :output "")))
        (messages nil))
    (cl-letf (((symbol-function 'dired-get-marked-files)
               (lambda () '("/tmp/good")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (chezmoi-dired-add-marked-files))
    (should-not messages)))

;;; chezmoi-dired-test.el ends here
