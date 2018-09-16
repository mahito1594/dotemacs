;;; init --- my init file
;;; Commentary:
;; My init file for Emacs ver 24 or later.
;; Use use-pacakage.el as pacakge installer.
;;
;; The following files are NOT tracked by git:
;;
;; * 09_font.el
;; * 91_singular.el
;; * 92_sage.el
;;
;; 09_font.el is for font setting:
;; (setq face-attribute 'default nil
;;                      :family <font-file-name>
;;                      :height <font-size>)
;;
;; 91_singular.el is for Running SINGULAR under Emacs.
;; For more detail, see
;;   https://www.singular.uni-kl.de/Manual/latest/sing_23htm#SEC30 :
;; (add-to-list 'load-path "<singular-emacs-home-directory>")
;; (autoload 'singular "singular"
;;   "Start Singular using default values." t)
;; (autoload 'singular-other "singular"
;;   "Aak for arguments and start Singular" t)
;;
;; 92_sage.el is for running SageMath under Emacs.
;; This contains
;;
;; - sage-shell-mode
;; - auto-complete-sage
;; - helm-sage
;;
;; For more detail, see indivisual README in GitHub.
;;
;;
;;
;; This init file uses init-loader.el;
;;
;; * 00 -- 09: init Emacs
;; * 10 -- 19: fundamental setting for programinngs
;; * 20 -- 89: settings for indivisual language
;; * 90 -- 98: computer algebra systems
;; * 99: keybindings
;;

;;; Code:
;; define the func to add load-path
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; add to load-path given dirs and its subdirs
(add-to-load-path "elisp" "conf" "public_repos")

;; install use-package
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  ;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/")t)
  (package-initialize)
  (unless package-archive-contents (package-refresh-contents))
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

;; init-loader
(use-package init-loader
  :config
  (setq init-loader-show-log-after-init 'error-only)
  (init-loader-load "~/.emacs.d/conf"))

(provide 'init)
;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
