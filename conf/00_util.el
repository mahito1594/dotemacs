;;; util.el --- utilities
;;; Commentary:
;; General settigns for utilities.
;;; Code:

;;;; Garbage Collection
(setq garbage-collection-messages t)    ; echo when GC run
(setq gc-cons-threshold (* gc-cons-threshold 10))

;;;; character encodings
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(use-package mozc
  ;; use mozc for GNU/Linux
  :if (eq system-type 'gnu/linux)
  :config
  (setq default-input-method "japanese-mozc")
  :bind
  (("C-j" . toggle-input-method)))
(use-package ucs-normalize
  ;; for macOS, settings about file names
  :straight nil
  :if (eq system-type 'darwin)
  :config
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

;;;; Backups and autosave files
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;;;; Set PATH
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;; mute beep sounds
(setq ring-bell-function 'ignore)

;;;; Highlights
;;; automatically insert close-paren
(electric-pair-mode 1)
(use-package paren
  ;; hilight corresponding parens
  :straight nil
  :config
  (show-paren-mode 1)
  (setq show-paren-style 'mixed))
;;; colorize delimeters
(use-package cl-lib)
(use-package color)
(use-package rainbow-delimiters
  :defer t
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
                           )))

;;;; dired
(put 'dired-find-alternate-file 'disabled nil)

;;;; Tree plugin: emacs-neotree
;; Type `M-x all-the-icons-install-fonts' at the first time.
;; After then, run `fc-cache -fv' in terminal.
(use-package all-the-icons)
(use-package neotree
  :after (all-the-icons)
  :defer t
  :bind (("C-c q" . neotree-toggle))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;;;; Helm
(use-package helm
  :blackout t
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :config
  (helm-mode 1)
  (use-package helm-swoop
    :bind (("M-i" . helm-swoop)
           ("M-I" . helm-swoop-back-to-last-point)
           ("C-c M-i" . helm-multi-swoop)
           ("C-x M-i" . helm-multi-swoop-all)
           :map isearch-mode-map
           ("M-i" . helm-swoop-from-isearch)
           :map helm-swoop-map
           ("C-r" . helm-previous-line)
           ("C-s" . helm-next-line)
           :map helm-multi-swoop-map
           ("C-r" . helm-previous-line)
           ("C-s" . helm-next-line))))

;;;; Hooks
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'after-init-hook 'server-start)

(provide '01_util)
;;; 01_util.el ends here
