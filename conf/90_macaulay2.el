;;; Macaulay 2
(when (file-exists-p "~/.emacs-Macaulay2")
  (load "~/.emacs-Macaulay2" t)
  (with-eval-after-load "M2"
    (define-key M2-comint-mode-map (kbd "<C-return>") 'M2-newline-and-indent))
  (add-hook 'm2-mode-hook '(whitespace-mode -1)))

(provide '90_macaulay2)
;;; 90_macaulay2.el ends here
