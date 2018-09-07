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

;; install init-loader.el
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  ;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/")t)
  (package-initialize)
  (unless (package-installed-p 'init-loader)
    (package-refresh-contents)
    (package-install 'init-loader))
  (require 'init-loader)
  (setq init-loader-show-log-after-init 'error-only)
  (init-loader-load "~/.emacs.d/conf"))

;; custom file
(setq custom-file "~/.emacs.d/custom-file.el")
(if (file-exists-p (expand-file-name "~/.emacs.d/custom-file.el"))
    (load (expand-file-name custom-file) t nil nil))
