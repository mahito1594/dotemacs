;;; 32_clang.el --- settings for C/C++

;; This program is released under the GPL v3.0 or,
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; At the first time, you must run
;; `M-x irony-install-server'.

;;; Code:

;; Fundamental
(use-package cc-mode
  :straight nil
  :config
  (use-package modern-cpp-font-lock
    :commands (modern-c++-font-lock-mode)
    :hook (c++-mode-hook . modern-c++-font-lock-mode)))

(use-package irony
  :hook (((c-mode c++-mode objc-mode) . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options))
  :config
  ;; Completion
  (use-package company-irony
    :demand t
    :after (company)
    :config
    (add-to-list 'company-backends 'company-irony))
  (use-package company-irony-c-headers
    :demand t
    :after (company-irony))
  ;; syntax check
  (use-package flycheck-irony
    :demand t
    :after (flycheck)
    :config
    (flycheck-irony-setup))
  ;; eldoc for irony
  (use-package irony-eldoc
    :hook (irony-mode . irony-eldoc)
    :blackout t)
  :blackout t)

(provide '32_clang)
;;; 32_clang.el ends here
