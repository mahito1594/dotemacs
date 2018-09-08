;;; auto complete

(use-package auto-complete
  :config
  (use-package auto-complete-config
    :ensure nil)
  (ac-config-default)
  (ac-set-trigger-key "TAB"))
