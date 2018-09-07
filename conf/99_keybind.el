;;; key binds
(define-key global-map (kbd "C-2") 'set-mark-command)
(define-key global-map (kbd "C-t") 'other-window)
(define-key key-translation-map [?\C-h] [?\C-?])

(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

(provide '99_keybind)
;;; 99_keybind.el ends here
