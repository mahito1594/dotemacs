;;; package install
;; (require 'package)
;; (add-to-list 'package-archives '("malpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (package-initialize)

;; package-name list to be installed
(defvar my-favorite-package-list
  '(
    ;; packages
    use-package package-utils

    ;; helm
    helm

    ;; PATH
    exec-path-from-shell

    ;; for display
    powerline dashboard hiwin rainbow-delimiters

    ;; spell check
    flycheck

    ;; auto complete
    auto-complete

    ;; python
    python-mode jedi py-autopep8
    ))

;; install-packages
(dolist (package-name my-favorite-package-list)
  (unless (package-installed-p package-name)
    (package-refresh-contents)
    (package-install package-name)))

;; use use-package
(require 'use-package)
