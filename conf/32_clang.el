;;; 32_clang.el --- settings for C/C++

;; This program is released under the GPL v3.0 or,
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; We use ccls, LSP for C/C++.  So you need install ccls.
;; See https://github.com/MaskRay/ccls/wiki

;;; Code:

(use-package cc-mode
  :straight nil
  :config
  (use-package modern-cpp-font-lock
    :commands (modern-c++-font-lock-mode)
    :hook (c++-mode-hook . modern-c++-font-lock-mode)
    :blackout t)
  (use-package ccls
    :hook ((c-mode c++-mode objc-mode) . (lambda ()
                                           (require 'ccls)
                                           (lsp)))
    :config
    (setq ccls-sem-highlight-method 'font-lock)
    (ccls-use-default-rainbow-sem-highlight)
    )
  )

(provide '32_clang)
;;; 32_clang.el ends here
