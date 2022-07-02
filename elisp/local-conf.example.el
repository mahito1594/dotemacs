;;; local-conf.el --- An example for local configuration

;;; Commentary:

;; This is an example for local configuration file `local-conf.el'.

;;; Code;

;; For debugging
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)
(setq max-lisp-eval-depth (* 2 max-lisp-eval-depth))

;; Font setting - an example
(defun my--font-setting ()
  (setq my-font-size 18
        my-font-family "Cica")
  (my--font-initialize))
(add-hook 'after-init-hook
          #'my--font-setting)

;; Set default frame size
(when (window-system)
  (defun my--framesize-setting ()
    (setq frame-initial-frame-alist
          (append (list '(width . 100)
                        '(height . 50)
                        '(top . 1)
                        '(left . 1))
                  frame-initial-frame-alist))
    (setq default-frame-alist frame-initial-frame-alist))
  (add-hook 'after-init-hook #'my--framesize-setting))

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
;; ;;; Example 1: dracula theme
;; (leaf dracula-theme
;;   :ensure t
;;   :require t
;;   :config
;;   (load-theme 'dracula t))

;; ;;; Example 2: sanityinc-tomorrow
;; (leaf color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (color-theme-sanityinc-tomorrow-day)  ; light theme
;;   ;; (color-theme-sanityinc-tomorrow-eighties) ; dark theme
;;   )

;;; Example 3: modus-themes
(leaf modus-themes
  :doc "
modus-theme is built-in since Emacs 28.
You can also install it by MELPA. See the official manual

  https://protesilaos.com/emacs/modus-themes

if necessary.
"
  :if (version<= "28" emacs-version)
  :ensure nil
  :config
  (load-theme 'modus-operandi)          ; light theme
  :custom
  (modus-themes-inhibit-reload . nil)
  (modus-themes-bold-constructs . nil)
  (modus-themes-italic-constructs . t)
  (modus-themes-syntax . '(green-strings alt-syntax))
  (modus-themes-mode-line . '(borderless accented)))

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
