;;; init.el --- init Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2019  TANNO Mahito

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This init file load `my-init.el'.  See `README.org'.

;;; Code:

;;; Debugging
(setq debug-on-error t)
(setq init-file-debug t)

;;; Load file
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;;; Variables and constants
(defconst my-minimum-emacs-version "25.4"
  "Expected minimum Emacs version.")
(defconst my-elisp-directory
  (expand-file-name "elisp" user-emacs-directory)
  "We should put here your self-made Emacs Lisp files.")
(defconst my-site-lisp-directory
  (expand-file-name "site-lisp" user-emacs-directory)
  "We should put here packages.")

;;; Load path
(add-to-list 'load-path my-elisp-directory) ; add `.emacs.d/elisp' to load-path
;; Add all subdirectories in `site-lisp' to load-path
(let ((default-directory my-site-lisp-directory))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;;; Load `my-init.el'.
(if (version< emacs-version my-minimum-emacs-version)
    (error (concat "Strich requires Emacs ver. %s or later, "
                   "but you use Emacs ver. %s!")
           my-minimum-emacs-version emacs-version)
  (require 'my-init))

;;; Set and load `custom-file'.
(setq custom-file (locate-user-emacs-file "custom-file.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
