;;; utility.el --- Some convenient functions for my Emacs configuration -*- lexical-binding: t -*-

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

;; This file is tangled from `README.org'.

;;; Code:

(defvar my-locate-readme (expand-file-name "README.org" user-emacs-directory)
  "Place where `README.org' should be.")

(defvar my-locate-init (expand-file-name "elisp/my-init.el" user-emacs-directory)
  "Place where `my-init.el' should be.")

(defvar my-locate-utility (expand-file-name "elisp/utility.el" user-emacs-directory)
  "Place where `utility.el' should be.")

(defvar my-locate-document (expand-file-name "doc/index.html" user-emacs-directory)
  "Place where `index.html' should be.")

(defun my-open-document ()
  "Open document in default browser."
  (interactive)
  (browse-url-of-file my-locate-document))

(defun my-edit-init ()
  "Open `README.org' to edit."
  (interactive)
  (find-file my-locate-readme))

(defun my-make-all ()
  "Make `my-init.el', `utility.el' and `index.html'."
  (interactive)
  (my-make-init-elisp)
  (byte-compile-file my-locate-utility)
  (my-make-document))

(defun my-make-init-elisp ()
  "Make `my-init.el' with `org-babel-tangle'."
  (interactive)
  (with-current-buffer (find-file-noselect my-locate-readme)
    (org-babel-tangle)))

(defun my-make-document ()
  "Make document html file."
  (interactive)
  (straight-use-package 'org)
  (straight-use-package 'htmlize)
  (with-current-buffer (find-file-noselect my-locate-readme)
    (setq-local org-html-htmlize-output-type 'css)
    (org-html-export-to-html)))

(defun my-load-config-files (re dir)
  "Load files which match to regular expression RE in DIR."
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)
    (dolist (file (my--pickup-config-files re dir))
      (condition-case err-var
          (load file)
        (error (message "%s" err-var))))))

(defun my--pickup-config-files (re dir)
  "Pick up files which match to regular expression RE in DIR."
  (let ((files (directory-files dir))
        (targets '()))
    (dolist (file files)
      (when (and (string-match re file)
                 (string-match "\\.el\\'" file))
        (push file targets)))
    (sort targets 'string<)))

(provide 'utility)
;;; utility.el ends here
