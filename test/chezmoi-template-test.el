;;; chezmoi-template-test.el --- Tests for chezmoi template display -*- lexical-binding: t -*-

(require 'ert)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))
(require 'chezmoi-template)

(ert-deftest chezmoi-template-execute-runs-locally-from-remote-buffer ()
  "Templates are executed with a local `default-directory' in TRAMP buffers."
  (let* ((seen-directory nil)
         (chezmoi-command-runner
          (lambda (_command)
            (setq seen-directory default-directory)
            (make-chezmoi--command-result :status 0 :output "value")))
         (default-directory "/ssh:example.com:/home/user/"))
    (should (equal "value" (chezmoi-template-execute "{{ .chezmoi.os }}")))
    (should-not (file-remote-p seen-directory))))

;;; chezmoi-template-test.el ends here
