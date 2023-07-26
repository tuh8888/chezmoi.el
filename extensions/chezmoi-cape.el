;;; chezmoi-company.el --- Company completion for chezmoi -*- lexical-binding: t -*-

;; Author: Harrison Pielke-Lombardo
;; Maintainer: Harrison Pielke-Lombardo
;; Version: 1.1.0
;; Package-Requires: ((emacs "27.1") (cape "0.34") (chezmoi "1.1.0"))
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

;; Provides a `cape' backend for `chezmoi'.

;;; Code:

(require 'chezmoi)
(require 'cape)

(defvar chezmoi-cape--properties
  (list :annotation-function (lambda (_) " Keyword")
	:company-kind (lambda (_) 'keyword)
	:exclusive 'no)
  "Completion extra properties for `chezmoi-cape'.")

(defun chezmoi-cape--next-keys (str)
  "Return candidates for STR for company completion.
Candidates are chezmoi data values corresponding to the path at point."
  (let* ((keys (thread-last chezmoi-template-key-regex
			    (split-string str)
			    butlast
			    (remove "")))
	 (hashget (lambda (m k) (gethash k m)))
	 (data (thread-last (chezmoi-get-data)
			    (cl-reduce hashget keys :initial-value))))
    (if (hash-table-p data)
	(hash-table-keys data)
      (list data))))

(defun chezmoi-cape--bounds ()
  "TODO."
  (let* ((bounds (cape--bounds 'word))
	 (beg (car bounds))
	 (end (cdr bounds)))
    (if (string-match "{{" (buffer-substring-no-properties beg end))
	(let* ((bounds (cape--bounds 'char))
	       (beg (car bounds))
	       (end (1- (cdr bounds))))
	  (cons beg end))
      bounds)))

(defun chezmoi-capf ()
  "Complete current template."
  (when (thing-at-point-looking-at chezmoi-template-regex)
    (let* ((candidates (thread-last 1
				    match-string
				    chezmoi-cape--next-keys
				    (apply-partially (lambda (c _) c)))))
      (let* ((bounds (chezmoi-cape--bounds))
	     (beg (car bounds))
	     (end (cdr bounds)))
	(buffer-substring-no-properties beg end)
	`(,beg ,end
	       ,(cape--table-with-properties
		 (cape--cached-table beg end candidates 'prefix)
		 :category 'cape-keyword)
	       ,@chezmoi-cape--properties)))))

(provide 'chezmoi-cape)

;;; chezmoi-cape.el ends here
