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

;;; Prevent GC to run in start-up
(defvar my--orig-gc-cons-threshold gc-cons-threshold
  "Original value of `gc-cons-threshold'.")

(setq gc-cons-threshold (* 10 1000 1000))

;; reset `gc-cons-threshold' after start up
(run-with-idle-timer 5 nil
                     (lambda ()
                       (setq gc-cons-threshold my--orig-gc-cons-threshold)
                       (message "The value of gc-cons-threshold is restored.")))

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

;;; Load `my-init.el'.
(if (version< emacs-version my-minimum-emacs-version)
    (error (concat "Emacs of ver. %s or later is required, "
                   "but you use Emacs of ver. %s!")
           my-minimum-emacs-version emacs-version)
  (add-to-list 'load-path my-elisp-directory)
  (require 'utility)
  (require 'my-init))

(provide 'init)
;;; init.el ends here
