;;; python-mode --- setting for Python
;;; Commentary:
;; Use judi for completion.
;; Type:
;;     M-x package-install RET jedi RET
;;     M-x jedi:install-server RET
;; Then open Python file.
;;
;; REQUIREMENT: virtualenv

;;; Code:

(use-package python-mode
  :commands python-mode
  :mode
  (("\\.py\\'" . python-mode)
   ("\\.sage\\'" . python-mode))
  :interpreter
  ("python" . python-mode)
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  :config
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (use-package py-autopep8
    :bind
    (:map python-mode-map
          ("C-c F" . py-autopep8)
          ("C-c f" . py-autopep8-region))))

(provide '31_python-mode)
;;; 31_python-mode.el ends here

