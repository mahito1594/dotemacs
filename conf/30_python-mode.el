;;; python-mode --- setting for Python
;;; Commentary:
;; Use jedi for completion.
;; You must install jedi by
;;    pip install jedi autopep8 virtualenv
;; For the first time only, you should type
;;    M-x jedi:install-server RET
;;
;; Requirement:
;;  * jedi
;;  * pylint
;;  * flake8
;;  * autopep8
;;  * virtualenv

;;; Code:
(use-package python
  ;; use build-in `python.el', NOT `python-mode.el'!
  :straight nil
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (when (executable-find "python3")
    ;; use python3 if it exists
    (setq python-shell-interpreter "python3")
    (setq flycheck-python-pylint-executable "python3")
    (setq flycheck-python-flake8-executable "python3"))
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (use-package company-jedi
    ;; completion by JEDI
    :config
    (add-to-list 'company-backends 'company-jedi)
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:complete-on-dot t)
    (setq jedi:use-shortcuts t))
  (use-package py-autopep8
    ;; use autopep8 for code format
    :bind (:map python-mode-map
                ("C-c F" . py-autopep8)
                ("C-c f" . py-autopep8-region))))

(provide '30_python-mode)
;;; 30_python-mode.el ends here
