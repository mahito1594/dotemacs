;;; flycheck:

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

(provide '13_flychecker)
;;; 13_flycheck.el ends here
