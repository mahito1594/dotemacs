;; Macaulay 2 start
(load "~/.emacs-Macaulay2" t)
(with-eval-after-load "M2"
  ; key binds
  (define-key M2-comint-mode-map (kbd "<C-return>") 'M2-newline-and-indent))
(add-hook 'm2-mode-hook '(whitespace-mode 0))
;; Macaulay 2 end
