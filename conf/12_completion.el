;;; completion --- auto completion
;;; Commentary:
;; I use `company' for autocompletion.
;; I got an error when I use :bind macro of use-package.
;; So, I define keybindings with `with-eval-after-load'.
;; I think, this is a bug.
;;; Code:

;; (use-package auto-complete
  ;; Deleted (2019-02-10)
  ;; :config
  ;; (use-package auto-complete-config
    ;; :straight nil)
  ;; (ac-config-default)
  ;; (ac-set-trigger-key "TAB")
  ;; (define-key ac-mode-map (kbd "M-TAB") 'auto-complete))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  ;; Switching from auto-complete. See
  ;; https://github.com/company-mode/company-mode/wiki/Switching-from-AC
  (with-eval-after-load 'company
    (define-key global-map (kbd "<tab>") 'compnay-indent-or-complete-common)
    (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
    (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous))
  :config
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (setq company-require-match 'never)
  )

(use-package company-quickhelp
  :if window-system
  :config
  (company-quickhelp-mode))

(provide '12_completion)
;;; 12_completion.el ends here
