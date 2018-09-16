;;; utilities

;; characters and input-method
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)


;; use mozc for GNU/Linux
(use-package mozc
  :if (eq system-type 'gnu/linux)
  :config
  (setq default-input-method "japanese-mozc")
  :bind
  (("C-j" . toggle-input-method)))

;; for macOS, settings about file names
(use-package ucs-normalize
  :ensure nil
  :if (eq system-type 'darwin)
  :config
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

;; package utilities
(use-package package-utils)

;; hook to give scrips permission
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; backups and autosave files
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; custom file
(setq custom-file "~/.emacs.d/custom-file.el")
(if (file-exists-p (expand-file-name "~/.emacs.d/custom-file.el"))
    (load (expand-file-name custom-file) t nil nil))

;; mute beep sounds
(setq ring-bell-function 'ignore)

;; auto insert close-paren
(electric-pair-mode 1)

;; don't use TAB for indent
(setq-default indent-tabs-mode nil)
