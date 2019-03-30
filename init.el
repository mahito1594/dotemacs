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

;; This init file
;;  1. insatlls `straight.el', a package manager, and
;;  2. load `strich.el', my Emacs configuration.
;; For more details, see `strich.org' or see `strich.html'.

;;; Code:
(setq debug-on-error t
      init-file-debug t)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(defvar strich-minimum-emacs-version "25.4")

(defvar strich-locate-strich-elisp-file (expand-file-name "strich.el" user-emacs-directory)
  "Place where `strich.el' should be.")

(defvar strich-locate-strich-org-file (expand-file-name "strich.org" user-emacs-directory)
  "Place where `strich.org' should be.")

(defvar strich-locate-strich-html-file (expand-file-name "doc/index.html" user-emacs-directory)
  "Place where html document shouled be.")

(if (version< emacs-version strich-minimum-emacs-version)
    (error (concat "Strich requires Emacs ver. %s or later, "
                   "but you use Emacs ver. %s!")
           strich-minimum-emacs-version emacs-version)
  (load strich-locate-strich-elisp-file))

(provide 'init)
;;; init.el ends here
