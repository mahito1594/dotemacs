;;; outline --- outline and hide/show

;; This program is released under the GPL v3.0 or,
;; (at your option) any later version.  See LICENSE.

;;; Commentary:
;;; Code:

;; ;;;; org-mode
;; ;; Install org-mode by git.
;; ;; The following code is given at
;; ;; https://github.com/raxod502/straight.el#installing-org-with-straightel
;; (require 'subr-x)
;; (straight-use-package 'git)
;; (defun org-git-version ()
;;   "The Git version of `org-mode'.
;; Inserted by installing `org-mode' or when a release is made."
;;   (require 'git)
;;   (let ((git-repo (expand-file-name
;;                    "straight/repos/org/" user-emacs-directory)))
;;     (string-trim
;;      (git-run "describe"
;;               "--match=release\*"
;;               "--abbrev=6"
;;               "HEAD"))))
;; (defun org-release ()
;;   "The release version of `org-mode'.
;; Inserted by installing `org-mode' or when a release is made."
;;   (require 'git)
;;   (let ((git-repo (expand-file-name
;;                    "straight/repos/org/" user-emacs-directory)))
;;     (string-trim
;;      (string-remove-prefix
;;       "release_"
;;       (git-run "describe"
;;                "--match=release\*"
;;                "--abbrev=0"
;;                "HEAD")))))
;; (provide 'org-version)

(use-package org
  :config
  ;; with electric-pair-mode:
  ;; We want to disable electric-pair-mode for `<' and `>'.
  ;; Also, we want to enable for `/' `~' and `='.
  (defvar my/org-electric-pair-pairs
    '((?/ . ?/) (?~ . ?~) (?= . ?=)))
  (defun my/org-electric-pair-inhibit-predicate (c)
    (if (char-equal ?<) t
      (electric-pair-default-inhibit c)))
  (defun my/org-electric-pair ()
    (setq-local electric-pair-pairs
                (append electric-pair-pairs my/org-electric-pair-pairs))
    (setq-local electric-pair-text-pairs
                (append electric-pair-text-pairs my/org-electric-pair-pairs))
    (setq-local electric-pair-inhibit-predicate
                #'my/org-electric-pair-inhibit-predicate))
  (add-hook 'org-mode-hook #'my/org-electric-pair)
  ;; For org-mode (ver 9.2 or later), we must load `org-temp.el'
  ;; in order to use easy templates.  Or, we should use
  ;; `org-insert-structure-template' binded to `C-c C-,'.
  (require 'org-tempo)
  (setq org-structure-template-alist
        (append '(("el" . "src emacs-lisp")
                  ("py" . "src python")
                  ("sh" . "src sh"))
                org-structure-template-alist))
  (setq org-src-fontify-natively t)
  ;; export to Github Flavored Markdown
  (use-package ox-gfm
    :demand t)
  )

;;;; outline
(use-package outline
  :straight nil
  :commands (outline-minor-mode)
  :config
  (use-package outline-magic
    :demand t
    :config
    (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle))
  :blackout outline-minor-mode)

;;;; hide/show
(use-package hideshow
  :straight nil
  :commands (hs-minor-mode)
  :bind ("C-+" . hs-toggle-hiding)
  :blackout hs-minor-mode)

(provide '02_outline)
;;; 02_outline.el ends here
