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
(use-package python-mode
  :defer t
  :commands (python-mode)
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  (setq py-install-directory (locate-user-emacs-file "straight/build/python-mode/"))
  :config
  (setq py-shell-name "python3")
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (use-package company-jedi
    ;; completion by JEDI
    :config
    (add-to-list 'company-backends 'company-jedi)
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:environment-virtualenv (list (locate-user-emacs-file ".python-environments/")))
    (setq jedi:complete-on-dot t)
    (setq jedi:use-shortcuts t))
  (use-package py-autopep8
    ;; use autopep8 for code format
    :bind (:map python-mode-map
                ("C-c F" . py-autopep8)
                ("C-c f" . py-autopep8-region))))

(provide '30_python-mode)
;;; 30_python-mode.el ends here
