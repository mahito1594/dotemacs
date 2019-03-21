;;; completion --- auto completion and syntax check

;; This program is released under the GPL v3.0 or,
;; (at your option) any later version.  See LICENSE.

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
  :hook (after-init . global-company-mode)
  :config
  ;; key bindings: Switching from auto-complete. See
  ;; https://github.com/company-mode/company-mode/wiki/Switching-from-AC
  ;; (define-key global-map (kbd "<tab>") 'compnay-indent-or-complete-common)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  ;; config
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (setq company-require-match 'never)

  ;;;; Gaphical interface:  documents and icons.
  (use-package company-quickhelp
    :demand t
    :if (window-system)
    :config
    (company-quickhelp-mode))
  (use-package company-box
    :if (and (window-system)
             (>= emacs-major-version 26))
    :hook (company-mode . company-box-mode)
    :custom
    (company-box-show-single-candidate t)
    (company-box-max-candidates 50)
    :config
    (setq company-box-backends-colors nil)
    ;; As a default, icons are slightly large.  See the issue
    ;; https://github.com/sebastiencs/company-box/issues/38
    (defvar my/company-box-icons-all-the-icons
      ;; In order to define following list, I referred to
      ;; GitHub: sebastiencs/company-box/company-box-icons.el (original), and
      ;; GitHub: seagle0128/.emacs.d/lisp/init-company.el (Centaur Emacs)
      `((Unknown       . ,(all-the-icons-faicon     "cog"                      :height 0.9))
        (Text          . ,(all-the-icons-octicon    "file-text"                :height 0.9))
        (Method        . ,(all-the-icons-faicon     "cube"                     :height 0.9))
        (Function      . ,(all-the-icons-faicon     "cube"                     :height 0.9))
        (Constructor   . ,(all-the-icons-faicon     "cube"                     :height 0.9))
        (Field         . ,(all-the-icons-faicon     "cog"                      :height 0.9))
        (Variable      . ,(all-the-icons-faicon     "cog"                      :height 0.9))
        (Class         . ,(all-the-icons-faicon     "cogs"                     :height 0.9))
        (Interface     . ,(all-the-icons-material   "share"                    :height 0.9))
        (Module        . ,(all-the-icons-alltheicon "less"                     :height 0.9))
        (Property      . ,(all-the-icons-faicon     "wrench"                   :height 0.9))
        (Unit          . ,(all-the-icons-material   "settings_system_daydream" :height 0.9))
        (Value         . ,(all-the-icons-material   "format_align_right"       :height 0.9))
        (Enum          . ,(all-the-icons-material   "content_copy"             :height 0.9))
        (Keyword       . ,(all-the-icons-material   "filter_center_focus"      :height 0.9))
        (Snippet       . ,(all-the-icons-material   "content_paste"            :height 0.9))
        (Color         . ,(all-the-icons-material   "palette"                  :height 0.9))
        (File          . ,(all-the-icons-faicon     "file"                     :height 0.9))
        (Reference     . ,(all-the-icons-material   "collections_bookmark"     :height 0.9))
        (Folder        . ,(all-the-icons-faicon     "folder"                   :height 0.9))
        (EnumMember    . ,(all-the-icons-material   "format_align_right"       :height 0.9))
        (Constant      . ,(all-the-icons-faicon     "square-o"                 :height 0.9))
        (Struct        . ,(all-the-icons-faicon     "cogs"                     :height 0.9))
        (Event         . ,(all-the-icons-faicon     "bolt"                     :height 0.9))
        (Operator      . ,(all-the-icons-material   "control_point"            :height 0.9))
        (TypeParameter . ,(all-the-icons-faicon     "cogs"                     :height 0.9))
        (Template      . ,(all-the-icons-material   "format_align_center"      :height 0.9))
        ))
    (setq company-box-icons-alist 'my/company-box-icons-all-the-icons)
    :blackout t)
  :blackout t)

;;;; Syntax checker
(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (use-package flycheck-pos-tip
    :demand t
    :config
    (flycheck-pos-tip-mode)))

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
