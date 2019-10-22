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
;; The original value of `gc-cons-threshold' is 800000.
(setq gc-cons-threshold (* 16 1000 1000))
(defvar my-gc-cons-threshold (* 8 100 1000)
  "Use this value as `gc-cons-threshold' after init Emacs.  You can modify the
value in \"local-conf.el\".")

;; reset `gc-cons-threshold' after start up
(run-with-idle-timer 5 nil
                     (lambda ()
                       (setq gc-cons-threshold my-gc-cons-threshold)
                       (setq garbage-collection-messages t)
                       (message "The value of gc-cons-threshold is set to %d" my-gc-cons-threshold)))

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
