;;; tree.el --- directory tree tab

;;; Code:
(use-package all-the-icons)
;; Type:
;;  M-x all-the-icons-install-fonts
;; After type, run `fc-cache -fv' in terminal.

(use-package neotree
  :bind (("C-c q" . neotree-toggle))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(provide '14_tree)
;;; 14_tree.el ends here
