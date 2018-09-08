;;; key binds
(define-key global-map (kbd "C-2") 'set-mark-command)
(define-key global-map (kbd "C-t") 'other-window)
(define-key global-map (kbd "C-x w") 'whitespace-mode)
(define-key key-translation-map [?\C-h] [?\C-?])

(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
