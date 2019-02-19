;;; python-mode --- setting for Python
;;; Commentary:

;; Use python-mode with LSP.
;; We must install python-language-server via pip:
;; $ pip install python-language-server[all]

;; We use code formatters:
;; - autopep8
;; We also must install them via pip.

;;; Code:
(use-package python-mode
  :defer t
  :mode "\\.py\\'"
  :interpreter "python"
  :init
  (setq py-shell-name "python3")
  :config
  (add-hook 'python-mode-hook #'lsp)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (use-package py-autopep8
    ;; use autopep8 for code format
    :bind (:map python-mode-map
                ("C-c F" . py-autopep8)
                ("C-c f" . py-autopep8-region))))

(provide '30_python-mode)
;;; 30_python-mode.el ends here
