;;; display.el --- settings for graphical part
;;; Commentary:
;; Settings for graphical user interface

;;; Code:
;;;; Startup-screen
(setq inhibit-startup-screen t)

;;;; Frame
(tool-bar-mode -1)                      ; hide tool bar
(scroll-bar-mode -1)                    ; hide scroll bar
(column-number-mode 1)                  ; show column number
(size-indication-mode 1)                ; show file size
(setq frame-title-format "%f")          ; show file path

;;;; Window
(set-frame-parameter nil 'fullscreen 'maximized) ; fullscreen

;;;; Color-theme
(use-package dracula-theme
  ;; Use Dracula thema, see
  ;; https://draculatheme.com/emacs/
  :demand t
  :config (load-theme 'dracula t))
(use-package powerline
  :demand t
  :config
  (powerline-default-theme))

(provide '01_display)
;;; 01_display.el ends here
