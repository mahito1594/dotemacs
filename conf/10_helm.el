;;; helm  --- settings for helm
;;; Commentary:
;; Replace some build-in commands with Helm's.
;;
;;; Code:

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :config
  (use-package helm-config
    :ensure nil)
  (helm-mode 1)
  )

(use-package helm-swoop
  :after (helm)
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)
         :map isearch-mode-map
         ("M-i" . helm-swoop-from-isearch)
         :map helm-swoop-map
         ("C-r" . helm-previous-line)
         ("C-s" . helm-next-line)
         :map helm-multi-swoop-map
         ("C-r" . helm-previous-line)
         ("C-s" . helm-next-line))
  )

(provide '10_helm)
;;; 10_helm.el ends here
