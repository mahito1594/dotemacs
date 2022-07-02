;;; local-conf.el --- An example for local configuration

;;; Commentary:

;; This is an example for local configuration file `local-conf.el'.

;;; Code;

;; For debugging
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

;; Font setting - an example
(add-hook 'after-init-hook
          #'(lambda ()
              (setq my-font-size 18
                    my-font-family "Cica")
              (my--font-initialize)))

;; Use Eclipse JDT LS ver. 0.57.0 with Java < 11
(leaf *eclipse.jdt.ls
  :ensure nil
  :after (lsp-java)
  :config
  (setq lsp-java-jdt-download-url
        "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz")
  (setq lsp-java-server-install-dir
        (locate-user-emacs-file ".cache/lsp/eclipse.jdt.ls-0.57.0/")))

;; Load your favarite theme
(leaf dracula-theme
  :ensure t
  :require t
  :config
  (load-theme 'dracula t))

;; When we are in LaTeX mode, turn on some features:
(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (display-line-numbers-mode 1)
              (whitespace-mode 1)
              (auto-fill-mode 1)
              (set-fill-column 80)))

;; Macaulay2 is a CAS, we use it for studying math.
(load "~/.emacs-Macaulay2" 'noerror)
(with-eval-after-load "M2"
  (define-key M2-comint-mode-map (kbd "C-<return>") #'M2-newline-and-indent))

;; Put some functions...and more
(defun my-insert-date ()
  "Insert current date of the form YYYY-MM-DD"
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(provide 'local-conf)
;;; local-conf.el ends here
