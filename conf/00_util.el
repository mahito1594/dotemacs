;;; util.el --- utilities

;; This program is released under the GPL v3.0 or,
;; (at your option) any later version.  See LICENSE.

;;; Commentary:
;; General settigns for utilities.

;;; Code:

;;;; character encodings
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(use-package mozc
  ;; use mozc for GNU/Linux
  :demand t
  :if (eq system-type 'gnu/linux)
  :config
  (setq default-input-method "japanese-mozc"))
(use-package ucs-normalize
  ;; for macOS, settings about file names
  :straight nil
  :demand t
  :if (eq system-type 'darwin)
  :config
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

;;;; Backups and autosave files
(defvar my-backup-directory
  (if (file-directory-p (locate-user-emacs-file "backups"))
      (setq my-backup-directory (locate-user-emacs-file "backups/"))
    (make-directory (locate-user-emacs-file "backups"))
    (setq my-backup-directory (locate-user-emacs-file "backups/")))
  "The directory which contains backup files.")
(setq backup-directory-alist
      `((".*" . ,my-backup-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,my-backup-directory t)))

;;;; Set PATH
(use-package exec-path-from-shell
  :demand t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;;;; mute beep sounds
(setq ring-bell-function 'ignore)

;;;; Highlights
;;; automatically insert close-paren
(electric-pair-mode 1)
(use-package paren
  ;; hilight corresponding parens
  :straight nil
  :demand t
  :config
  (show-paren-mode 1)
  (setq show-paren-style 'mixed))
;;; colorize delimeters
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
;;; highlight for region
(transient-mark-mode 1)
(use-package hiwin
  ;; de-emphasize non-active windows
  :config
  (hiwin-activate)
  (set-face-background 'hiwin-face "DarkSlateGray"))

;;;; Whitespaces and tabs
(setq-default indent-tabs-mode nil)     ; Don't use tabs for indent
(use-package whitespace
  ;; visualize whitespaces/tabs
  :defer t
  :commands (whitespace-mode)
  :bind (("C-c w" . whitespace-mode))
  :config
  (setq whitespace-style '(
                           face
                           trailing
                           tabs
                           spaces
                           empty
                           space-mark
                           tab-mark
                           ))
  :blackout t)

;;;; dired
(use-package dired
  :straight nil
  :demand t
  :config
  (put 'dired-find-alternate-file 'disabled nil))
(use-package all-the-icons-dired
  :if (window-system)
  :hook (dired-mode . all-the-icons-dired-mode))

;;;; Tree plugin: emacs-neotree
;; Type `M-x all-the-icons-install-fonts' at the first time.
;; After then, run `fc-cache -fv' in terminal.
(use-package all-the-icons
  :if (display-graphic-p)
  :demand t)
(use-package neotree
  :bind (("C-c q" . neotree-toggle))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;;;; Ivy, Counsel and Swiper
(use-package counsel
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-S-s" . swiper-all)
         ("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("<tab>" . ivy-alt-done)
         ("C-w" . ivy-yank-word))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-wrap t)
  (ivy-format-function 'ivy-format-function-arrow)
  (counsel-yank-pop "\n---------\n")
  (ivy-initial-inputs-alist nil)
  :config
  (use-package amx
    :demand t)
  (use-package ivy-rich
    ;; Show icons when you swich buffers, or find files.
    ;; See https://github.com/Yevgnen/ivy-rich#13-customization
    :preface
    (defun my/ivy-rich-buffer-icon (candidate)
      "Show buffer isons in `ivy-rich', only on GUI."
      (when (display-graphic-p)
        (with-current-buffer
            (get-buffer candidate)
          (let ((icon (all-the-icons-icon-for-mode major-mode)))
            (if (symbolp icon)
                (all-the-icons-icon-for-mode 'fundamental-mode)
              icon)))))
    (defun my/ivy-rich-file-icon (candidate)
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
    :demand t
    :custom
    (ivy-rich-path-style 'abbrev)
    (ivy-rich-display-transformers-list
     '(ivy-switch-buffer
       (:columns
        ((my/ivy-rich-buffer-icon :width 2)
         (ivy-rich-candidate (:width 30))
         (ivy-rich-switch-buffer-size (:width 7))
         (ivy-rich-switch-buffer-indicators (:width 4 :face error :align left))
         (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
         (ivy-rich-switch-buffer-project (:width 15 :face success))
         (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
        :predicate
        (lambda (cand) (get-buffer cand)))
       counsel-M-x
       (:columns
        ((counsel-M-x-transformer (:width 40))
         (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
       counsel-describe-function
       (:columns
        ((counsel-describe-function-transformer (:width 40))
         (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
       counsel-describe-variable
       (:columns
        ((counsel-describe-variable-transformer (:width 40))
         (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
       counsel-recentf
       (:columns
        ((ivy-rich-candidate (:width 0.8))
         (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
       counsel-find-file
       (:columns
        ((my/ivy-rich-file-icon :width 2)
         (ivy-rich-candidate)))
       counsel-git
       (:columns
        ((my/ivy-rich-file-icon :width 2)
         (ivy-rich-candidate)))))
    :config
    (ivy-rich-mode +1)
    :blackout t)
  :blackout t)

(use-package which-key
  :hook (after-init . which-key-mode)
  :bind (:map which-key-mode-map
              ("C-x DEL" . which-key-C-h-dispatch))
  :custom
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  :blackout t)

;;;; Hooks
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'after-init-hook 'server-start)

(provide '00_util)
;;; 00_util.el ends here
