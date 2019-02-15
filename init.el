;;; init --- my init file
;;; Commentary:
;; My init file for Emacs ver 25.3 or later.
;; Use straight.el and use-pacakage.el as pacakge installer.
;;

;;; Code:
(defun add-to-load-path (&rest paths)
  "This function add the given `PATHS' to `load-path' recursively."
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; add to load-path given dirs and its subdirs
(add-to-load-path "elisp" "conf" "public_repos")

;;;; Straight.el
;; install straight.el, see
;; https://github.com/raxod502/straight.el#getting-started
(if (< emacs-major-version 25.3)
    ;; Require Emacs ver 25.3 or later. If you use an old one,
    ;; `use-package' do nothing.
    (defmacro use-package (&rest args))
  (setq straight-check-for-modifications 'live-with-find) ; => '(check-on-save find-when-checking)
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (straight-use-package 'el-patch)
  (straight-use-package 'use-package)
  (setq straight-use-package-by-default t))

;;;; Blackout
(use-package blackout
  ;; hide some major/minor mode in the mode line
  :straight (:host github :repo "raxod502/blackout"))

;;;; init-loader
(use-package init-loader
  :config
  (setq init-loader-show-log-after-init 'error-only)
  (init-loader-load "~/.emacs.d/conf"))

;; custom file
(setq custom-file "~/.emacs.d/custom-file.el")
(if (file-exists-p (expand-file-name "~/.emacs.d/custom-file.el"))
    (load (expand-file-name custom-file) t nil nil))

(provide 'init)
;;; init.el ends here
