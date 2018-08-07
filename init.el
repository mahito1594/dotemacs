;; define the func to add load-path
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; add to load-path given dirs and its subdirs
(add-to-load-path "elisp" "conf" "public_repos")


;; characters and input-method
(prefer-coding-system 'utf-8)
(set-language-environment "Japanese")
(when (eq system-type 'gnu/linux)
  (require 'mozc)
  (setq default-input-method "japanese-mozc")
  (define-key global-map (kbd "C-j") 'toggle-input-method))

;; for macOS, settings about file names
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))


;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; package-name list to be installed
(defvar my-favorite-package-list
  '(
    ;; helm
    helm

    ;; for display
    powerline dashboard hiwin rainbow-delimiters

    ;; auto complete
    auto-complete
    ))

;; install packages
(dolist (package-name my-favorite-package-list)
  (unless (package-installed-p package-name)
    (package-refresh-contents)
    (package-install package-name)))


;; mute beep sound
(setq ring-bell-function 'ignore)

;; startup-screen
(setq inhibit-startup-screen t)
(require 'dashboard)
(dashboard-setup-startup-hook)
;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))) ; show dashboard when call emacsclient

;; hide tool-bar
(tool-bar-mode 0)

;; hide scroll-bar
(scroll-bar-mode 0)

;; display column number
(column-number-mode 1)

;; display file size
(size-indication-mode 1)

;; display full-path of the file
(setq frame-title-format "%f")

;; hilight current line
(defface my-hl-line-face
  ;; if backgroud is dark, use gray
  '((((class color) (background dark))
     (:background "Gray16" t))
    ;; if background is light, use green
    (((class color) (background light))
     (:background "LightGoldenYellow" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode 1)

;; hilight for corresponding parens
(setq show-paren-deley 0)
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; auto insert close-paren
(electric-pair-mode 1)

;; rainbow delimeters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(require 'cl-lib)
(require 'color)
(defun rainbow-delimiters-using-stronger-colors ()
  (interactive)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
    (cl-callf color-saturate-name (face-foreground face) 30))))
(add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors)

;; show spaces and tabs
(require 'whitespace)
(setq whitespace-style '(
                         ;; face
                         ;; trailing
                         tabs
                         spaces
                         ;; empty
                         space-mark
                         tab-mark
                         ))
(global-whitespace-mode 1)

;; don't use TAB for indent
(setq-default indent-tabs-mode nil)

;; hide non-active windows
(require 'hiwin)
(hiwin-activate)
(set-face-background 'hiwin-face "Gray50")


;; helm
(require 'helm)
(require 'helm-config)
(helm-mode 1)


;; auto complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(ac-set-trigger-key "TAB")



;; my key bind
(define-key global-map (kbd "C-2") 'set-mark-command)
(define-key global-map (kbd "C-t") 'other-window)

;; hooks
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


;; set color thema
(load-theme 'misterioso t)

;; powerline
(require 'powerline)
(powerline-default-theme)

;; font setting
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil
                      :family "Source Han Code JP"
                      :height 140))
(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil
                      :family "Source Han Code JP"
                      :height 110))


;; back-ups and auto-save file
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))


;; Macaulay 2 start
(load "~/.emacs-Macaulay2" t)
(with-eval-after-load "M2"
  ; key binds
  (define-key M2-comint-mode-map (kbd "<C-return>") 'M2-newline-and-indent))
;; Macaulay 2 end
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rainbow-delimiters hiwin dashboard auto-complete powerline helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
