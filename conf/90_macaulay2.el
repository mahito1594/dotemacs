;;; M2 --- Macualay2: Computer Algebra System

;; This program is released under the GPL v3.0 or,
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; Macaulay2 (M2) is available at
;; http://www2.macaulay2.com/Macaulay2/

;;; Code:

(when (file-exists-p "~/.emacs-Macaulay2")
  (load "~/.emacs-Macaulay2" t)
  (with-eval-after-load "M2"
    (define-key M2-comint-mode-map (kbd "<C-return>") 'M2-newline-and-indent))
  (add-hook 'm2-mode-hook '(whitespace-mode -1)))

(provide '90_macaulay2)
;;; 90_macaulay2.el ends here
