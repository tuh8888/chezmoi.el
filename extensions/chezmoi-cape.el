;;; chezmoi-company.el --- Company completion for chezmoi -*- lexical-binding: t -*-

;; Author: Harrison Pielke-Lombardo
;; Maintainer: Harrison Pielke-Lombardo
;; Version: 1.2.0
;; Package-Requires: ((emacs "26.1") (chezmoi "1.2.0"))
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

;; Obsolete.  `chezmoi-capf' now lives in `chezmoi' and needs no Cape.
;; `chezmoi-mode' adds it to `completion-at-point-functions' for you.

;;; Code:

(require 'chezmoi)

(display-warning 'chezmoi
		 "`chezmoi-cape' is obsolete; `chezmoi-capf' is now part of `chezmoi'.  Remove (require 'chezmoi-cape) from your config.")

(provide 'chezmoi-cape)

;;; chezmoi-cape.el ends here
