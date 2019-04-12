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

(defvar my-locate-document (expand-file-name "docs/index.html" user-emacs-directory)
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
  (my-make-init)
  (byte-compile-file my-locate-utility)
  (my-make-document))

(defun my-make-init ()
  "Make `my-init.el' with `org-babel-tangle'."
  (interactive)
  (with-current-buffer (find-file-noselect my-locate-readme)
    (org-babel-tangle)))

(defun my-make-document ()
  "Make document html file."
  (interactive)
  (with-current-buffer (find-file-noselect my-locate-readme)
    (org-html-export-to-html)))

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

(defun my--outline-move-subtree-down (&optional arg)
  "Move the currrent subtree down past ARG headlines of the same level.
If the current subtree is folded, call `outline-hide-subtree' after move down."
  (interactive "p")
  (let* ((headers (or arg 1))
         (movfunc (if (> headers 0) 'outline-get-next-sibling
                    'outline-get-last-sibling))
         (ins-point (make-marker))
         (cnt (abs headers))
         (folded (save-match-data
                   (outline-end-of-heading)
                   (outline-invisible-p)))
         beg end txt)
    ;; Select the tree
    (outline-back-to-heading)
    (setq beg (point))
    (outline-end-of-subtree)
    (if (= (char-after) ?\n) (forward-char 1))
    (setq end (point))
    ;; Find insertion point, with error handling
    (goto-char beg)
    (while (> cnt 0)
      (or (funcall movfunc)
          (progn (goto-char beg)
                 (error "Cannot move past superior level")))
      (setq cnt (1- cnt)))
    (if (> headers 0)
        ;; Moving forward - still need to move over subtree
        (progn (outline-end-of-subtree)
               (if (= (char-after) ?\n) (forward-char 1))))
    (move-marker ins-point (point))
    (setq txt (buffer-substring beg end))
    (delete-region beg end)
    (insert txt)
    (goto-char ins-point)
    (if folded (outline-hide-subtree))
    (move-marker ins-point nil)))

;;; For YaTeX: integrate with outline-minor-mode
;; The following code is a modification a part of `tex-mode.el'
;; which is bundled with GNU Emacs.
;; Copyright (C) 1985-1986, 1989, 1992, 1994-1999, 2001-2019 Free
;; Software Foundation, Inc.
;; Released under the GPL v3.0 or any later version.

(defvar my-YaTeX-section-alist
  '(("part" . 0)
    ("chapter" . 1)
    ("section" . 2)
    ("subsection" . 3)
    ("subsubsection" . 4)
    ("paragraph" . 5)
    ("subparagraph" . 6)))

(defvar my-YaTeX-metasection-list
  '("documentclass"
    "begin{document}" "end{document}"
    "frontmatter" "mainmatter" "appendix" "backmatter"))

(defvar my-YaTeX-outline-regexp
  (concat (regexp-quote "\\")
          (regexp-opt (append my-YaTeX-metasection-list
                              (mapcar #'car my-YaTeX-section-alist))
                      t)))

(defvar my-YaTeX-outline-promotion-headings
  '("\\chapter" "\\section" "\\subsection"
    "\\subsubsection" "\\paragraph" "\\subparagraph"))

(defun my-YaTeX-outline-level ()
  (if (looking-at my-YaTeX-outline-regexp)
      (1+ (or (cdr (assoc (match-string 1) my-YaTeX-section-alist)) -1))
    1000))

(defun my-YaTeX-with-outline ()
  (outline-minor-mode 1)
  (setq-local outline-regexp my-YaTeX-outline-regexp)
  (setq-local outline-level #'my-YaTeX-outline-level)
  (setq-local outline-promotion-headings my-YaTeX-outline-promotion-headings))

;;; For Ebib
(defun my-ebib-name-transform-function (key)
  "Serach file of the form
       SEARCH-DIRS/FIRST-AUTHOR/ENTRY-KEY"
  (format "%s/%s"
          (substring key (string-match "[A-Za-z]+" key) (match-end 0))
          (replace-regexp-in-string ":" "" key)))

(provide 'utility)
;;; utility.el ends here
