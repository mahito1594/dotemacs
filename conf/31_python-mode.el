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
;;  * autopep8
;;  * virtualenv

;;; Code:

(use-package company-jedi
  :config
  (setq jedi:environment-virtualenv (list (expand-file-name ".~/.emacs.d/.python-environments/")))
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (add-hook 'python-mode-hook
            (lambda () (add-to-list 'company-backends 'company-jedi))))

(use-package python-mode
  :commands python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (setq indent-tabs-mode nil)
  (setq tab-width 4))

(use-package py-autopep8
  :bind
  (:map python-mode-map
        ("C-c F" . py-autopep8)
        ("C-c f" . py-autopep8-region)))

(provide '31_python-mode)
;;; 31_python-mode.el ends here
