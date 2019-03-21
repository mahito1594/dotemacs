;;; display.el --- settings for graphical part

;; This program is released under the GPL v3.0 or,
;; (at your option) any later version.  See LICENSE.

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
(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-neotree-file-icons t)
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (find-file-visit-truename t)
  :config
  (setq doom-modeline-mu4e nil
        doom-modeline-irc nil))

(provide '01_display)
;;; 01_display.el ends here
