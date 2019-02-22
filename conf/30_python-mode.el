;;; python-mode --- setting for Python

;; This program is released under the GPL v3.0 or,
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; Use python-mode with LSP.
;; We must install python-language-server via pip:
;; $ pip install python-language-server[all]

;; We use code formatters:
;; - autopep8
;; We also must install them via pip.

;;; Code:
(use-package python
  ;; use build-in `python.el', NOT `python-mode.el'!
  :straight nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (add-hook 'python-mode-hook #'lsp)
  (when (executable-find "python3")
    ;; use python3 if it exists
    (setq python-shell-interpreter "python3")
    (setq flycheck-python-pylint-executable "python3")
    (setq flycheck-python-flake8-executable "python3"))
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (use-package py-autopep8
    ;; use autopep8 for code format
    :demand t
    :bind (:map python-mode-map
                ("C-c F" . py-autopep8)
                ("C-c f" . py-autopep8-region))))

(provide '30_python-mode)
;;; 30_python-mode.el ends here
