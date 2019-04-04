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

;;; for ivy-rich: show icons
(defun my-ivy-rich-buffer-icon (candidate)
  "Show buffer isons in `ivy-rich', only on GUI."
  (when (display-graphic-p)
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
          icon)))))

(defun my-ivy-rich-file-icon (candidate)
  "Show file icons in `ivy-rich', only on GUI."
  (when (display-graphic-p)
    (let ((icon
           ;; for directories
           (if (file-directory-p candidate)
               (cond
                ;; for `tramp-mode'
                ((and (fboundp 'tramp-tramp-file-p)
                      (tramp-tramp-file-p default-directory))
                 (all-the-icons-octicon "file-directory"))
                ;; for symbolic links
                ((file-symlink-p candidate)
                 (all-the-icons-octicon "file-symlink-directory"))
                ;; for git submodules
                ((all-the-icons-dir-is-submodule candidate)
                 (all-the-icons-octicon "file-submodule"))
                ;; for version-controled by git
                ((file-exists-p (format "%s/.git" candidate))
                 (all-the-icons-octicon "repo"))
                ;; otherwise
                (t (let ((matcher (all-the-icons-match-to-alist candidate all-the-icons-dir-icon-alist)))
                     (apply (car matcher) (list (cadr matcher))))))
             ;; for files
             (all-the-icons-icon-for-file candidate))))
      (unless (symbolp icon)
        (propertize icon
                    'face `(:family ,(all-the-icons-icon-family icon) :height 1.1))))))

;;; for Org-mode: integrate with electric-pair-mode
(defvar my-org-electric-pair-pairs
  '((?~ . ?~) (?= . ?=)))

(defun my-org-electric-pair-inhibit (char)
  "Do not insert close `>'."
  (if (char-equal char ?<)
      t
    (electric-pair-default-inhibit char)))
(defun my-org-electric-pair-mode ()
  "Use Org-mode with electric-pair-mode."
  (electric-pair-mode +1)
  (setq-local electric-pair-pairs (append electric-pair-pairs
                                          my-org-electric-pair-pairs))
  (setq-local electric-pair-text-pairs (append electric-pair-text-pairs
                                               my-org-electric-pair-pairs))
  (setq-local electric-pair-inhibit-predicate #'my-org-electric-pair-inhibit))

(provide 'utility)
;;; utility.el ends here
