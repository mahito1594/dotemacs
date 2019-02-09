;;; completion --- auto completion
;;; Commentary:
;;; Codes:

(use-package auto-complete
  :config
  (use-package auto-complete-config
    :straight nil)
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete))

(provide '12_completion)
;;; 12_completion.el ends here
