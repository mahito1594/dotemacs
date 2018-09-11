;;; key binds
(define-key global-map (kbd "C-2") 'set-mark-command)
(define-key global-map (kbd "C-t") 'other-window)
(define-key global-map (kbd "C-x w") 'whitespace-mode)
(define-key global-map (kbd "C-c t") 'toggle-truncate-lines)
(define-key global-map (kbd "C-;") 'comment-line)

;; C-h => backspace, backspace => help
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(define-key key-translation-map (kbd "DEL") (kbd "C-h"))

(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
