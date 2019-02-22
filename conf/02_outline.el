;;; outline --- outline and hide/show

;; This program is released under the GPL v3.0 or,
;; (at your option) any later version.  See LICENSE.

;;; Commentary:
;;; Code:

;;;; org-mode
;; Install org-mode by git.
;; The following code is given at
;; https://github.com/raxod502/straight.el#installing-org-with-straightel
(require 'subr-x)
(straight-use-package 'git)
(defun org-git-version ()
  "The Git version of `org-mode'.
Inserted by installing `org-mode' or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))
(defun org-release ()
  "The release version of `org-mode'.
Inserted by installing `org-mode' or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))
(provide 'org-version)
(use-package org
  :config
  ;; for org-mode (ver 9.2 or later), we should load `org-temp.el'
  ;; in order to use easy templates.
  (require 'org-tempo)
  (setq org-structure-template-alist
        (append '(("el" . "src emacs-lisp")
                  ("py" . "src python")
                  ("sh" . "src sh"))
                org-structure-template-alist))
  (setq org-src-fontify-natively t)
  (use-package ox-gfm                   ; export to GitHub Flavored Markdown
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
