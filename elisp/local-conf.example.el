;;; local-conf.example.el --- An example for local configuration

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
                    my-font-family "Cica"
                    all-the-icons-scale-factor 0.85)
              (my--font-initialize)))

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

(provide 'local-conf.example)
;;; local-conf.example.el ends here
