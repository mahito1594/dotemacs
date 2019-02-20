;;; completion --- auto completion and syntax check

;;; Commentary:
;; I use `company' for autocompletion.
;; I got an error when I use :bind macro of use-package.
;; So, I define keybindings with `with-eval-after-load'.
;; I think, this is a bug.

;; Use Language Server Protocol for completion, and so on, via lsp-mode, see
;; https://github.com/emacs-lsp/lsp-mode
;; To use lsp for a Language XXX, add
;; (add-hook 'XXX-mode-hook #'lsp)

;;; Code:

;;;; Auto complete
(use-package company
  :commands (global-company-mode)
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  ;; key bindings: Switching from auto-complete. See
  ;; https://github.com/company-mode/company-mode/wiki/Switching-from-AC
  (define-key global-map (kbd "<tab>") 'compnay-indent-or-complete-common)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  ;; config
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (setq company-require-match 'never)
  (use-package company-quickhelp
    :demand t
    :if window-system
    :config
    (company-quickhelp-mode))
  :blackout t)

;;;; Syntax checker
(use-package flycheck
  :commands (global-flycheck-mode)
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (use-package flycheck-pos-tip
    :demand t
    :config
    (flycheck-pos-tip-mode))
  :blackout t)

;;;; Language Server Protocol
(use-package lsp-mode
  :commands (lsp)
  :config
  (use-package company-lsp
    :demand t
    :config
    (push 'company-lsp company-backends)))
(use-package lsp-ui
  :commands (lsp-ui-mode)
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  )

(provide '12_completion)
;;; 12_completion.el ends here
