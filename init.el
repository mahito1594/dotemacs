;;; init.el --- init Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2019--2022  Mahito Tanno

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

;;; Code:

;;; Prevent GC to run in start-up
;; The original value of `gc-cons-threshold' is 800000.
(defvar my-gc-cons-threshold gc-cons-threshold
  "Use this value as `gc-cons-threshold' after init Emacs.  You can modify the
value in \"local-conf.el\".")
(setq gc-cons-threshold (* 16 1000 1000)) ; increase the value when start-up

;; reset `gc-cons-threshold' after start up
(run-with-idle-timer 5 nil
                     (lambda ()
                       (setq gc-cons-threshold my-gc-cons-threshold)
                       (setq garbage-collection-messages t)
                       (message "The value of gc-cons-threshold is set to %d"
                                my-gc-cons-threshold)))

;;; Debugging
(setq debug-on-error t)
(setq init-file-debug t)

;;; Fundamental variables, constants and functions
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (expand-file-name "elisp/" user-emacs-directory))

(defconst my-minimum-emacs-version "26.1"
  "Expected minimum Emacs version.  However, we strongly recommend using Emacs
version 26.3 or higher.")


;;; Load configuration files.
(if (version< emacs-version my-minimum-emacs-version)
    (error (concat "Emacs of ver. %s or later is required, "
                   "but you use Emacs of ver. %s!")
           my-minimum-emacs-version emacs-version)
  (require 'config)
  (require 'local-conf nil t))

(provide 'init)
;;; init.el ends here
