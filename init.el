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


;; set character code
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; for macOS, settings about file names
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))


;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;; hide startup-screen
(setq inhibit-startup-screen t)

;; hide tool-bar
(tool-bar-mode 0)

;; hide scroll-bar
(scroll-bar-mode 0)

;; display column number
(column-number-mode t)

;; display file size
(size-indication-mode t)

;; display full-path of the file
(setq frame-title-format "%f")

;; hilight current line
(defface my-hl-line-face
  ;; if backgroud is dark, use gray
  '((((class color) (background dark))
     (:background "gray16" t))
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

;; show spaces and tabs
(require 'whitespace)
(setq whitespace-style '(face
                         ;; trailing
                         tabs
                         spaces
                         empty
                         space-mark
                         tab-mark
                         ))
(global-whitespace-mode t)

;; don't use TAB for indent
(setq-default indent-tabs-mode nil)

;; my key bind
(define-key global-map (kbd "C-2") 'set-mark-command)
(define-key global-map (kbd "C-t") 'other-window)

;; hooks
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


;; set color thema
(load-theme 'misterioso t)

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
;; Macaulay 2 end
