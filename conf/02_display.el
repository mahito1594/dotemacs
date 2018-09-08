;;; settings for display

;; startup-screen
(setq inhibit-startup-screen t)
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

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

;; set color-thema
(load-theme 'misterioso t)

;; powerline
(use-package powerline
  :config
  (powerline-default-theme))

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

;; rainbow delimeters
(use-package rainbow-delimiters
  :hook
  (prog-mode-hook . rainbow-delimiters-mode)
  :config
  (use-package cl-lib)
  (use-package color)
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
                           ;; face
                           ;; trailing
                           tabs
                           spaces
                           ;; empty
                           space-mark
                           tab-mark
                           ))
  (global-whitespace-mode 1))

;; hide non-active windows
(use-package hiwin
  :config
  (hiwin-activate)
  (set-face-background 'hiwin-face "Gray50"))
