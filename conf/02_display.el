;;; display.el --- settings for graphical part
;;; Commentary:
;; Settings for graphical user interface

;;; Code:
;; startup-screen
(setq inhibit-startup-screen t)

;; hide tool-bar
(tool-bar-mode -1)

;; hide scroll-bar
(scroll-bar-mode -1)

;; display column number
(column-number-mode 1)

;; display file size
(size-indication-mode 1)

;; display full-path of the file
(setq frame-title-format "%f")

;; mazimize screen size on startup
(set-frame-parameter nil 'fullscreen 'maximized)

;; set color-thema
;; (load-theme 'misterioso t)
(use-package dracula-theme
  :config (load-theme 'dracula t))

;powerline
(use-package powerline
  :config
  (powerline-default-theme))

;; hilight for corresponding parens
(use-package paren
  :straight nil
  :config
  (show-paren-mode 1)
  (setq show-paren-style 'mixed))

;; highlight for region
(transient-mark-mode 1)

;; rainbow delimeters
(use-package cl-lib)
(use-package color)
(use-package rainbow-delimiters
  :after (cl-lib color)
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :init
  (defun rainbow-delimiters-using-stronger-colors ()
    (interactive)
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 30))))
  (add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors))

;; show whitespace and tab
(use-package whitespace
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
  (global-whitespace-mode -1))

;; hide non-active windows
(use-package hiwin
  :config
  (hiwin-activate)
  (set-face-background 'hiwin-face "DarkSlateGray"))

(provide '02_display)
;;; 02_display.el ends here
