
;; python-mode setting
;; REQUIREMENT: virtualenv

(use-package python-mode
  :commands python-mode
  :mode
  (("\\.py\\'" . python-mode)
   ("\\.sage\\'" . python-mode))
  :interpreter
  ("python" . python-mode)
  :config
  (setq indent-tabs-mode nil)
  (use-package py-autopep8
    :init
    (bind-keys :map python-mode-map
               ("C-c F" . py-autopep8)
               ("C-c f" . py-autopep8-region))))

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:compolete-on-dot t) ; optional

(provide '31_python-mode)
;;; 31_python-mode.el ends here

