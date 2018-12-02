;;; key binds
(define-key global-map (kbd "C-m") 'newline-and-indent)
(define-key global-map (kbd "C-2") 'set-mark-command)
(define-key global-map (kbd "C-t") 'other-window)
(define-key global-map (kbd "C-x w") 'whitespace-mode)
(define-key global-map (kbd "C-c t") 'toggle-truncate-lines)
(define-key global-map (kbd "C-;") 'comment-line)

;; C-h => backspace, C-x ? => help for help
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(define-key global-map (kbd "C-x ?") 'help-for-help)

;; bibtex-mode
(with-eval-after-load "bibtex"
  (define-key bibtex-mode-map (kbd "C-j") nil)
  (define-key bibtex-mode-map (kbd "<C-return>") 'bibtex-next-field))

(provide '99_keybind)
;;; 99_keybind.el ends here
