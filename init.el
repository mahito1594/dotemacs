;;; init --- my config file for GNU Emacs

;; Copyright (C) 2019  TANNO Mahito

;; Author: Mahito Tanno

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
;; My init file for Emacs ver 25.3 or later.
;; Use straight.el and use-pacakage.el as pacakge installer.

;; straight.el is released under the MIT License
;; copyright (C) 2017 Radon Rosborough
;; https://github.com/raxod502/straight.el

;;; Code:

;;;; Garbage Collection
(setq garbage-collection-messages t)    ; echo when GC run
(setq gc-cons-threshold (* gc-cons-threshold 250))

;;;; Re setting `user-emacs-directory'
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;;;; Straight.el
;; install straight.el, see
;; https://github.com/raxod502/straight.el#getting-started
(if (< emacs-major-version 25.3)
    ;; Require Emacs ver 25.3 or later. If you use an old one,
    ;; `use-package' do nothing.
    (defmacro use-package (&rest args))
  (setq straight-check-for-modifications 'live-with-find) ; => '(check-on-save find-when-checking)
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (straight-use-package 'el-patch)
  (straight-use-package 'use-package)
  (setq straight-use-package-by-default t)
  (setq use-package-always-defer t)
  )

;;;; Blackout
(use-package blackout
  ;; hide some major/minor mode in the mode line
  :straight (:host github :repo "raxod502/blackout")
  :demand t)

;;;; init-loader
(use-package init-loader
  :demand t
  :config
  (setq init-loader-show-log-after-init 'error-only)
  (init-loader-load "~/.emacs.d/conf"))

;;;; custom file
(setq custom-file "~/.emacs.d/custom-file.el")
(if (file-exists-p (expand-file-name "~/.emacs.d/custom-file.el"))
    (load (expand-file-name custom-file) t nil nil))

(provide 'init)
;;; init.el ends here
