;;; auto complete

(use-package auto-complete
  :config
  (use-package auto-complete-config
    :ensure nil)
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete))
