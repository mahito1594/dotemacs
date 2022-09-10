;;; config.el --- My configuration file for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2020--2022  Mahito Tanno

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;; Setup
;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    ;; (leaf el-get :ensure t)             ; I do not use el-get now...
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

;;; Fundamental
(leaf leaf
  :config
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf cus-edit
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf *Fundamental
  :config
  ;; Backup files
  (defvar my-backup-directory (expand-file-name "backups/"
                                                user-emacs-directory))
  (setq backup-directory-alist `((".*" . ,my-backup-directory))
        auto-save-file-name-transforms `((".*" ,my-backup-directory t))
        auto-save-list-file-prefix (concat my-backup-directory "/.saves-"))

  (setq ring-bell-function 'ignore)     ; ignore ring bell when error occure

  ;; Language/Codings
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)

  ;; Add newlien at the end of file
  (setq require-final-newline t)

  ;; Don't delete *scratch* and *Messages* buffer
  (with-current-buffer "*scratch*" (emacs-lock-mode 'kill))
  (with-current-buffer "*Messages*" (emacs-lock-mode 'kill))

  ;; *Help* buffer will be automatically selected
  (setq help-window-select t)

  ;; Set Option Key to META
  (when (eq window-system 'mac)
    (setq mac-option-modifier 'meta))

  ;; By the following line, Emacs can save files in WSL
  (when (eq system-type 'windows-nt)
    (defun fp/ignore-wsl-acls (orig-fun &rest args)
      "This workaround is given in

https://github.com/microsoft/WSL/issues/6004#issuecomment-880576799

Ignore ACLs on WSL. WSL does not provide an ACL, but emacs
expects there to be one before saving any file. Without this
advice, files on WSL can not be saved."
      (if (or (string-match-p "^//wsl\$/" (car args))
              (string-match-p "^//wsl.localhost/" (car args)))
          (progn (message "ignoring wsl acls") "")
        (apply orig-fun args)))

    (advice-add 'file-acl :around 'fp/ignore-wsl-acls))

  (leaf exec-path-from-shell
    :doc "Copy PATH if GUI"
    :if (memq window-system '(mac ns x))
    :ensure t
    :require t
    :config
    (exec-path-from-shell-initialize))

  (leaf ucs-normalize
    :doc "If we use macOS, there are some problem about filename encoding."
    :if (eq system-type 'darwin)
    :custom ((locale-coding-system . 'utf-8-hfs))
    :require t
    :config
    (set-file-name-coding-system 'utf-8-hfs))

  (leaf server
    :preface
    (defun my--server-start ()
      (unless (and (fboundp 'server-running-p)
                   (server-running-p))
        (server-start)))
    :hook ((after-init-hook . my--server-start))))

(leaf *Utilities
  :config
  (setq-default indent-tabs-mode nil)   ; Use SPACE instead of TAB
  (add-hook 'after-init-hook #'transient-mark-mode) ; Highlight current region

  (leaf *Parenthesis
    :config
    (leaf elec-pair
      :doc "Automatically insert paired delimiters."
      :hook ((after-init-hook . electric-pair-mode)))

    (leaf paren
      :doc "Enhance corresponding paren."
      :hook ((after-init-hook . show-paren-mode))
      :custom ((show-paren-style . 'mixed)))

    (leaf rainbow-delimiters
      :ensure t
      :hook ((prog-mode-hook . rainbow-delimiters-mode))))

  (leaf whitespace
    :doc "Visible SPACEs and TABs. You can toggle by `C-c t w'."
    :commands (whitespace-mode)
    :bind (("C-c t w" . whitespace-mode))
    :custom ((whitespace-style . '(
                                   face
                                   trailing
                                   tabs
                                   spaces
                                   empty
                                   space-mark
                                   tab-mark
                                   ))))

  (leaf restart-emacs
    :doc "You can restart Emacs by typing `M-x restart-emacs'."
    :ensure t
    :commands (restart-emacs))

  (leaf esup
    :doc "Emacs Start UP"
    :ensure t
    :commands (esup))

  (leaf beacon
    :doc "Enhance current line."
    :ensure t
    :hook ((after-init-hook . beacon-mode))
    :custom ((beacon-color . "yellow")))

  (leaf symbol-overlay
    :doc "Highlight same symbols where cursor is."
    :ensure t
    :bind (("M-i" . symbol-overlay-put))
    :hook ((prog-mode-hook . symbol-overlay-mode)))

  (leaf *Dired
    :config
    (leaf dired
      :custom ((dired-recursive-copies . 'always))
      :config
      (put 'dired-find-alternate-file 'disabled nil))

    (leaf dired-x
      :after (dired)
      :bind ((:dired-mode-map
              ("." . hydra-dired/body)))
      :custom ((dired-omit-files . "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$"))
      :require t
      :hydra (hydra-dired
              (:hint nil)
              "
^Navigate^          ^Edit^            ^Mark^               ^Command^           ^Misc^
^^^^^^^^^^-----------------------------------------------------------------------------------------
_n_: next           _+_: mkdir        _m_: mark            _Z_: compress file  _(_: details
_p_: previous       _C_: copy         _u_: unmark          ^ ^                 _)_: hide some files
_J_: up directory   _R_: rename       _U_: unmark all      ^ ^                 _g_: refresh
^ ^                 _D_: delete       _t_: toggle marks    _M_: chmod
_f_: open file      ^ ^               _E_: extension mark  _G_: chgrp          _q_: quit window
_v_: view file      _Y_: rel symlink  _F_: find marked     _O_: chown
_a_: open in        _S_: symlink
^ ^    current buf  ^ ^               ^ ^                  _!_: shell command  _._: toggle Hydra
"
              ;; Navigate
              ("n" dired-next-line)
              ("p" dired-previous-line)
              ("g" revert-buffer)
              ("J" dired-up-directory)
              ("f" dired-find-file)
              ("v" dired-view-file)
              ("a" dired-find-alternate-file)
              ;; Edit
              ("+" dired-create-directory)
              ("C" dired-do-copy)
              ("R" dired-do-rename)
              ("D" dired-do-delete)
              ("Y" dired-do-relsymlink)
              ("S" dired-do-symlink)
              ;; Mark
              ("m" dired-mark)
              ("u" dired-unmark)
              ("U" dired-unmark-all-marks)
              ("t" dired-toggle-marks)
              ("E" dired-mark-extension)
              ("F" dired-do-find-marked-files)
              ("Z" dired-do-compress)
              ("M" dired-do-chmod)
              ("G" dired-do-chgrp)
              ("O" dired-do-chown)
              ("!" dired-do-shell-command)
              ;; Misc
              ("(" dired-hide-details-mode)
              (")" dired-omit-mode)
              ("g" revert-buffer)
              ("q" quit-window)
              ("." nil)))
    )

  (leaf *MinibufferCompletion
    :config
    (leaf vertico
      :doc "Minibuffer Completion UI"
      :ensure t
      :advice
      (:around vertico--format-candidate
               (lambda (orig cand prefix suffix index _start)
                 (setq cand (funcall orig cand prefix suffix index _start))
                 (concat
                  (if (= vertico--index index)
                      (propertize "» " 'face 'vertico-current)
                    "  ")
                  cand)))
      :init
      (vertico-mode 1)
      :custom ((vertico-cycle . t)
               (enable-recursive-minibuffers . t))
      :config
      (savehist-mode 1))

    (leaf orderless
      :doc "We use `orderless' completion style"
      :ensure t
      :custom ((completion-styles . '(orderless basic))
               (completion-category-default . nil)
               (completion-category-overrides . '((file (styles basic partial-completion))))))

    (leaf marginalia
      :doc "Rich annotations in the minibuffer"
      :ensure t
      :bind (("M-A" . marginalia-cycle)
             (:minibuffer-local-map
              ("M-A" . marginalia-cycle)))
      :init
      (marginalia-mode 1))

    (leaf consult
      :doc "Useful search/navigation commands"
      :ensure t
      :bind (;; C-x bindings
             ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
             ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
             ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
             ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
             ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
             ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
             ;; Other custom bindings
             ("M-y" . consult-yank-pop)                ;; orig. yank-pop
             ;; M-g bindings (goto-map)
             ("M-g e" . consult-compile-error)
             ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
             ("M-g g" . consult-goto-line)             ;; orig. goto-line
             ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
             ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
             ("M-g m" . consult-mark)
             ("M-g k" . consult-global-mark)
             ("M-g i" . consult-imenu)
             ("M-g I" . consult-imenu-multi)
             ;; M-s bindings (search-map)
             ("M-s d" . consult-find)
             ("M-s D" . consult-locate)
             ("M-s g" . consult-grep)
             ("M-s G" . consult-git-grep)
             ("M-s r" . consult-ripgrep)
             ("M-s l" . consult-line)
             ("M-s L" . consult-line-multi)
             ("M-s m" . consult-multi-occur)
             ("M-s k" . consult-keep-lines)
             ("M-s u" . consult-focus-lines)
             ;; Override default i-search by consult-line
             ("C-s" . consult-line)
             ("C-r" . consult-line)
             ;; ;; Isearch integration
             ;; ("M-s e" . consult-isearch-history)
             ;; (:isearch-mode-map
             ;;  ("M-e" . consult-isearch-history)        ;; orig. isearch-edit-string
             ;;  ("M-s e" . consult-isearch-history)      ;; orig. isearch-edit-string
             ;;  ("M-s l" . consult-line)                 ;; needed by consult-line to detect isearch
             ;;  ("M-s L" . consult-line-multi))          ;; needed by consult-line to detect isearch
             ;; ;; Minibuffer history
             (:minibuffer-local-map
              ("M-s" . consult-history)                ;; orig. next-matching-history-element
              ("M-r" . consult-history))
             )
      :hook (completion-list-mode-hook . consult-preview-at-point-mode)
      :advice
      (:override completing-read-multiple consult-completing-read-multiple)
      :init
      (setq xref-show-xrefs-function #'consult-xref
            xref-show-definitions-function #'consult-xref)
      :config
      (consult-customize consult-ripgrep consult-git-grep consult-grep
                         consult-bookmark consult-recent-file consult-xref
                         consult--source-bookmark consult--source-recent-file
                         consult--source-project-recent-file
                         :preview-key (kbd "M-."))
      (consult-customize consult-theme
                         :preview-key
                         (list (kbd "M-.")
                               :debounce 0.5 (kbd "<up>") (kbd "<down>")
                               :debounce 1 'any)))

    (leaf affe
      :doc "Asynchronous fuzzy finder

For some unkown reasons, `affe' does not work well on Windows."
      :unless (eq system-type 'windows-nt)
      :ensure t
      :after consult
      :config
      (consult-customize affe-grep :preview-key (kbd "M-."))
      (defun affe-orderless-regexp-compiler (input _type _ignorecase)
        (setq input (orderless-pattern-compiler input))
        (cons input (lambda (str) (orderless--highlight input str))))
      (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))

    (leaf consult-ghq
      :doc "GHQ interface using Consult/Affe

`consult-find' / `affe-find' does not work well on Windows.
So, I override some functions."
      :if (executable-find "ghq")
      :ensure t
      :preface
      (when (eq system-type 'windows-nt)
        (defun my-consult-ghq-open ()
          "Open local repository under ghq"
          (interactive)
          (let* ((repo (consult--read (consult-ghq--list-candidates) :prompt "Repo ")))
            (find-file repo))))
      :config
      (when (eq system-type 'windows-nt)
        (advice-add 'consult-ghq-find :override #'my-consult-ghq-open)
        (advice-add 'consult-ghq-grep :override #'my-consult-ghq-open)))

    (leaf embark
      :doc "Minibuffer actions and context menus"
      :ensure t
      :bind (("C-." . embark-act)
             ("C-;" . embark-dwin))
      :init
      (setq prefix-help-command #'embark-prefix-help-command)
      :config
      (leaf embark-consult
        :ensure t
        :hook ((embark-collect-mode-hook . consult-preview-at-point-mode)))))

  (leaf which-key
    :doc "Show keybindings"
    :ensure t
    :bind ((:which-key-mode-map
            ("C-x DEL" . which-key-C-h-dispatch)
            ("C-c DEL" . which-key-C-h-dispatch)))
    :hook ((after-init-hook . which-key-mode))
    :custom ((which-key-popup-type . 'side-window)
             (which-key-side-window-location . 'bottom))
    :blackout t))


;;; Completion and Syntax checking
(leaf *Completion
  :config
  (leaf yasnippet
    :ensure t
    :hook ((after-init-hook . yas-global-mode))
    :blackout t)

  (leaf *Company
    :config
    (leaf company
      :ensure t
      :bind ((:company-active-map
              ("<backtab>" . company-select-previous)
              ("<tab>" . company-complete-common-or-cycle)))
      :hook ((after-init-hook . global-company-mode))
      :custom ((company-idle-delay . 0)
               (company-require-match . 'never)
               (company-text-icons-add-background . t))
      :blackout t)

    (leaf company-quickhelp
      :if (window-system)
      :ensure t
      :hook ((company-mode-hook . company-quickhelp-mode)))))

(leaf *SyntaxChecking
  :config
  (leaf *Flymake
    :config
    (leaf flymake-diagnostic-at-point
      :ensure t
      :commands (flymake-diagsnotic-at-point-mode)
      :hook ((flymake-mode-hook . flymake-diagnostic-at-point-mode))))

  (leaf *Flycheck
    :config
    (leaf flycheck
      :ensure t
      :commands (flycheck-disable-checker)
      :custom ((flycheck-disabled-checkers . '(emacs-lisp-checkdoc))))

    (leaf consult-flycheck
      :ensure t
      :commands (consult-flycheck))

    (leaf flycheck-popup-tip
      :ensure t
      :hook ((flycheck-mode-hook . flycheck-popup-tip-mode)))))

(leaf editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(leaf *LanguageServer
  :config
  (leaf lsp-mode
    :preface
    (setq lsp-keymap-prefix "s-l")
    :ensure t
    :commands (lsp lsp-deferred)
    :hook ((lsp-mode . lsp-enable-which-key-integration)))

  (leaf consult-lsp
    :ensure t
    :bind ((:lsp-mode-map
            ([remap xref-find-apropos] . consult-lsp-symbols))))

  (leaf lsp-ui
    :ensure t
    :commands (lsp-ui-mode)
    :bind ((:lsp-ui-mode-map
            ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
            ([remap xref-find-references] . lsp-ui-peek-find-refrences)))
    :blackout t))

;;; Programing language
(leaf web-mode
  :ensure t
  :mode ("\\.phtml\\'" "\\.tpl\\.php" "\\.[agj]sp\\'"
         "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'"
         "\\.djhtml\\'" "\\.xml\\'" "\\.html?\\'"))

(leaf js-mode
  :doc "Use typescript-language-server.

See
  https://emacs-lsp.github.io/lsp-mode/page/lsp-typescript/
if necessary."
  :if (executable-find "npm")
  :ensure nil
  :hook ((js-mode-hook . lsp)))

(leaf lsp-java
  :doc "Edit Java using Language Server: eclipse.jdt.ls.

If you're using Java < 11, you must download jdt-language-server of version 0.57.0."
  :if (executable-find "java")
  :ensure t
  :hook ((java-mode-hook . lsp)))

;;; Documents
(leaf *Org-mode
  :doc "Orgmode settings"
  (leaf org
    :preface
    ;; Config for electric pair mode in Org
    (defvar my-org-electric-pair-pairs
      '((?~ . ?~) (?= . ?=)))

    (defun my-org-electric-pair-inhibit (char)
      "Do not insert close `>'"
      (if (char-equal char ?>)
          t
        (electric-pair-default-inhibit char)))

    (defun my-org-electric-pair-mode ()
      (electric-pair-mode 1)
      (setq-local electric-pair-pairs (append electric-pair-pairs
                                              my-org-electric-pair-pairs))
      (setq-local electric-pair-text-pairs (append electric-pair-pairs
                                                   my-org-electric-pair-pairs))
      (setq-local electric-pair-inhibit-predicate #'my-org-electric-pair-inhibit))
    :hook ((org-mode-hook . my-org-electric-pair-mode))
    :custom ((org-startup-indented . t)
             (org-fontify-natively . t)
             (org-html-htmlize-output-type . 'css))
    :config
    (setq org-structure-template-alist (append '(("el" . "src emacs-lisp"))
                                               org-structure-template-alist))

    (leaf toc-org
      :doc "Generate TOC.

To generate TOC, put a `:TOC:' tag at the first headline."
      :ensure t
      :commands (toc-org-mode))

    (leaf ox-gfm
      :ensure t
      :after (ox))

    (leaf htmlize
      :ensure t
      :after (ox)))

  (leaf org-bullets
    :ensure t))

(leaf *TeX/LaTeX
  :doc "TeX/LaTeX settings"
  :when (executable-find "tex")
  :config
  (defvar my-locate-texmfhome
    (substring (shell-command-to-string "kpsewhich -var-val TEXMFHOME")
    0 -1)
    "Place where TEXMFHOME should be.  For deail about TDS (TeX Directory
System), see TeX Wiki.")

  (leaf *AUCTeX
    :ensure auctex
    :config
    (leaf tex
      :preface
      (defun my-plain-TeX-mode-hook ()
        "My personal hook for TeX mode."
        (outline-minor-mode 1)
        (setq-local TeX-electric-math
                    (cons "$" "$"))
        ;; Run TexLab --- a language server for LaTeX --- if available
        (when (executable-find "texlab")
          (lsp)))
      :hook ((plain-TeX-mode-hook . my-plain-TeX-mode-hook))
      :init
      (setq TeX-format-list
            '(("JLATEX" japanese-latex-mode
               "\\\\\\(documentstyle\\|documentclass\\)[^%\n]*{\\(u\\|lt\\|bx\\)?\\(j[st-]?\\|t\\)\
\\(article\\|report\\|book\\|slides\\|lreq\\)")
              ("JTEX" japanese-plain-tex-mode
               "-- string likely in Japanese TeX --")
              ("AMSTEX" ams-tex-mode
               "\\\\document\\b")
              ("CONTEXT" context-mode
               "\\\\\\(start\\(text\\|tekst\\|proje[ck]t\\|proiect\\|\
produ[ck]t\\|produs\\|environment\\|omgeving\\|umgebung\\|prostredi\\|mediu\\|\
component\\|onderdeel\\|komponent[ea]\\|componenta\\)\
\\|inizia\\(testo\\|progetto\\|prodotto\\|ambiente\\|componente\\)\
\\)\\|%.*?interface=")
              ("LATEX" latex-mode
               "\\\\\\(begin\\|\\(?:sub\\)\\{0,2\\}section\\|chapter\\|documentstyle\\|\
documentclass\\)\\b")
              ("TEX" plain-tex-mode ".")))
      :custom
      ((TeX-auto-save . nil)
       (TeX-parse-self . t)
       (TeX-electric-sub-and-superscript . t)
       (TeX-source-correlate-mode . t)
       (TeX-source-correlate-method . '((dvi . synctex)
                                        (pdf . synctex)))
       (TeX-expand-list . '(("%(bibtex)"
                             (lambda ()
                               (cond
                                ((eq TeX-engine 'ptex)
                                 (if (executable-find "pbibtex")
                                     "pbibtex %(kanjiopt)" "jbibtex"))
                                ((eq TeX-engine 'jtex) "jbibtex")
                                ((and japanese-TeX-mode
                                      (memq TeX-engine '(uptex xetex luatex)))
                                 "upbibtex")
                                (t "bibtex")))))))
      :config
      (add-to-list 'TeX-command-list '("LatexMk" "latexmk %t"
                                       TeX-run-TeX nil
                                       (latex-mode) :help "Run latexmk")))

    (leaf latex
      :preface
      (defun my-LaTeX-remove-eqnarray-from-environments ()
        "Remove \"eqnarray\" and \"eqnarray*\" from the variable
`LaTeX-environment-list'.  We must not use them.  For detail, see Madsen's
report (\"Avoid eqnarray!\").

This function is due to A. Esbati. see
https://tex.stackexchange.com/questions/320524/how-to-deactivate-eqnarray-environment-in-auctex"
        (let ((evil-envs '("eqnarray" "eqnarray*")))
          (LaTeX-environment-list)
          (dolist (env evil-envs)
            (setq-local LaTeX-environment-list
                        (assq-delete-all
                         (car (assoc env LaTeX-environment-list))
                         LaTeX-environment-list)))))

      (defun my-LaTeX-mode-hook ()
        (my-LaTeX-remove-eqnarray-from-environments)
        (outline-minor-mode 1)
        (electric-pair-local-mode -1)
        (setq-local TeX-electric-math
                    (cons "\\(" "\\)"))
        ;; Run TexLab --- language server for LaTeX --- if available and *not*
        ;; in japanese-(La)TeX-mode.  Otherwise, use company-math package.
        (if (and (executable-find "texlab")
                 (or (not (boundp 'japanese-TeX-mode))
                     (not japanese-TeX-mode)))
            (lsp)
          (setq-local company-backends
                      (append
                       '(company-math-symbols-latex company-latex-commands)
                       company-backends))))

      (defun my--disable-insert-LaTeX-label (name &optional type no-insert)
        "Do not insert label by AUCTeX.  We add labels manually.

Remark: a blank line will be inserted after \"\begin{...}\".  I don't know how
to inhibit this behavior."
        (list name type 'no-insert))

      (defun my-LaTeX-label-cleanup ()
        "Set `LaTeX-label-alist' to nil."
        (setq LaTeX-label-alist nil))
      :hook ((LaTeX-mode-hook . my-LaTeX-mode-hook)
             (TeX-auto-cleanup-hook . my-LaTeX-remove-eqnarray-from-environments)
             (TeX-auto-cleanup-hook . my-LaTeX-label-cleanup))
      ;; :advice
      ;; (:filter-args LaTeX-label my--disable-insert-LaTeX-label)
      :custom ((LaTeX-section-label . nil)
               (LaTeX-electric-left-right-brace . t)))

    (leaf font-latex
      :custom ((font-latex-fontify-script . nil)))

    (leaf tex-jp
      :preface
      (defun my-japanese-LaTeX-guess-engine ()
        "Guess Japanese TeX engine and set it to `TeX-engine'.
Document class and its option is considered in the guess.  Do not
overwrite the value already set locally."
        ;; `TeX-engine' may be set by the file local variable or by the menu
        ;; Command->TeXing Options manually.  Don't override the user
        ;; preference set in such ways.
        (unless (local-variable-p 'TeX-engine (current-buffer))
          (TeX-engine-set
           (cond
            ((TeX-match-style "jlreq")
             (cond
              ((LaTeX-match-class-option "\\`platex\\'") 'ptex)
              ((LaTeX-match-class-option "\\`uplatex\\'") 'uptex)
              ((LaTeX-match-class-option "\\`lulatex\\'") 'luatex)
              (t japanese-TeX-engine-default)))
            ((TeX-match-style "\\`bxjs\\(?:article\\|report\\|book\\)\\'")
             (cond
              ((LaTeX-match-class-option "\\`platex\\'") 'ptex)
              ((LaTeX-match-class-option "\\`uplatex\\'") 'uptex)
              ((LaTeX-match-class-option "\\`lualatex\\'") 'luatex)
              ((LaTeX-match-class-option "\\`xelatex\\'") 'xetex)
              (t japanese-TeX-engine-default)))
            ((TeX-match-style "\\`ltj[st]?\\(?:article\\|report\\|book\\)\\'")
             'luatex)
            ((TeX-match-style "\\`u[jt]\\(?:article\\|report\\|book\\)\\'")
             'uptex)
            ((TeX-match-style "\\`[jt]s?\\(?:article\\|report\\|book\\)\\'")
             (if (LaTeX-match-class-option "\\`uplatex\\'")
                 'uptex 'ptex))
            ((TeX-match-style "\\`j-\\(?:article\\|report\\|book\\)\\'")
             'jtex)
            (t japanese-TeX-engine-default)))))
      :advice
      (:override japanese-LaTeX-guess-engine my-japanese-LaTeX-guess-engine)
      :custom
      ((japanese-TeX-engine-default . 'uptex)
       (japanese-LaTeX-default-style . "jsarticle")
       (japanese-LaTeX-style-list . '(("jsarticle") ("jsreport") ("jsbook")
                                      ("ujarticle") ("ujreport") ("ujbook")
                                      ("utarticle") ("utreport") ("utbook")
                                      ("ltjarticle") ("ltjreport") ("ltjbook")
                                      ("ltjsarticle") ("ltjsreport")
                                      ("ltjsbook")
                                      ("bxjsarticle") ("bxjsreport") ("bjsbook")
                                      ("bxjsslide")
                                      ("jlreq"))))))

  (leaf company-math
    :ensure t
    :after (company)
    :require t)

  (leaf reftex
    :hook ((LaTeX-mode-hook . reftex-mode))
    :custom
    ((reftex-plug-into-AUCTeX . t)
     (reftex-ref-style-default-list . '("Cleveref"))
     (reftex-label-alist . '((nil ?e nil "~\\ref{%s}" nil nil) ; omit parens surrounding eq-like reference
                             ("definition"  ?d "def:"  "~\\ref{%s}" nil ("definition")  nil)
                             ("Definition"  ?d "def:"  "~\\ref{%s}" nil ("definition")  nil)
                             ("notation"    ?d "not:"  "~\\ref{%s}" nil ("notation")    nil)
                             ("Notation"    ?d "not:"  "~\\ref{%s}" nil ("notation")    nil)
                             ("proposition" ?p "prop:" "~\\ref{%s}" nil ("proposition") nil)
                             ("Proposition" ?p "prop:" "~\\ref{%s}" nil ("proposition") nil)
                             ("theorem"     ?p "thm:"  "~\\ref{%s}" nil ("theorem")     nil)
                             ("Theorem"     ?p "thm:"  "~\\ref{%s}" nil ("theorem")     nil)
                             ("lemma"       ?p "lem:"  "~\\ref{%s}" nil ("lemma")       nil)
                             ("Lemma"       ?p "lem:"  "~\\ref{%s}" nil ("lemma")       nil)
                             ("corollary"   ?p "cor:"  "~\\ref{%s}" nil ("corollary")   nil)
                             ("Corollary"   ?p "cor:"  "~\\ref{%s}" nil ("corollary")   nil)
                             ("remark"      ?r "rem:"  "~\\ref{%s}" nil ("remark")      nil)
                             ("Remark"      ?r "rem:"  "~\\ref{%s}" nil ("remark")      nil)
                             ("example"     ?x "ex:"   "~\\ref{%s}" nil ("example")     nil)
                             ("Example"     ?x "ex:"   "~\\ref{%s}" nil ("example")     nil)
                             ("conjecture"  ?c "conj:" "~\\ref{%s}" nil ("conjecture")  nil)
                             ("Conjecture"  ?c "conj:" "~\\ref{%s}" nil ("conjecture")  nil)))
     (reftex-use-external-file-finders . t)
     (reftex-external-file-finders . '(("tex" . "kpsewhich -format=.tex %f")
                                       ("bib" . "kpsewhich -format=.bib %f")))
     (reftex-bibliography-commands . '("bibliography"
                                       "nobibliography"
                                       "addbibresource"))))

  (leaf bibtex
    :bind ((:bibtex-mode-map
            ("C-j" . nil)
            ("C-<return>" . bibtex-next-field)))
    :mode (("\\.bib\\'" . bibtex-mode))
    :custom
    ((bibtex-user-option-fileds . '(("yomi" "Yomigana")
                                    ("MRNUMBER" "Math. Rev. Number")
                                    ("archivePrefix" "name of the preprint server")
                                    ("eprint" "Electric print")
                                    ("shortjournal" "Journal abbreviation")))
     (bibtex-autokey-name-case-convert-function . 'capitalize)
     (bibtex-autokey-titleword-case-convert-function . 'capitalize)
     (bibtex-autokey-titleword-separator . "")
     (bibtex-autokey-titleword-length . nil)
     (bibtex-autokey-titlewords . 1)
     (bibtex-autokey-year-length . 4)
     (bibtex-autokey-year-title-separator . ":")
     (bibtex-autokey-titleword-ignore . '("A" "An" "On" "The" "a" "an" "on" "the"
                                          "Le" "La" "Les" "le" "la" "les"
                                          "Zur" "zur"))))

  (leaf ebib
    :doc "Ebib is a BibTeX database manager."
    :ensure t
    :commands (ebib)
    :bind ((:ebib-multiline-mode-map
            ("C-c C-c" . ebib-quit-multiline-buffer-and-save)))
    :init
    (customize-set-variable 'ebib-preload-bib-files
                            (list
                             (expand-file-name "bibtex/bib/articles.bib" my-locate-texmfhome)
                             (expand-file-name "bibtex/bib/books.bib" my-locate-texmfhome)
                             (expand-file-name "bibtex/bib/others.bib" my-locate-texmfhome))
                            "Customized with leaf in ebib block")
    (customize-set-variable 'ebib-file-associations
                            (cond
                             ((eq system-type 'darwin)
                              '(("pdf" . "open")
                                ("ps" . "open")))
                             (t
                              '(("pdf" . "xpdf")
                                ("ps" . "gv"))))
                            "Customized with leaf in ebib block")
    :custom
    `((ebib-bibtex-dialect . 'BibTeX)
      (ebib-extra-fileds . '((BibTeX "crossref"
                                     "annote"
                                     "keywords"
                                     "doi"
                                     "shortjournal"
                                     "archivePrefix" "eprint" "primaryClass"
                                     "MRCLASS" "MRNUMBER"
                                     "file")
                             (biblatex "crossref"
                                       "annotation"
                                       "keywords"
                                       "shortjournal"
                                       "archivePrefix" "primaryClass"
                                       "MRCLASS" "MRNUMBER"
                                       "file")))
      (ebib-file-search-dirs . '("~/BibFile/Papers"
                                 "~/BibFile/Books"
                                 "~/BibFile/Proceedings"))
      (ebib-name-transform-function . #'my-ebib-name-transform-function)
      (ebib-keywords-file . ,(expand-file-name "emacs/ebib-keywords.txt" my-locate-texmfhome))
      (ebib-keywords-use-only-file . t)
      (ebib-keywords-field-keep-sorted . t)
      (ebib-keywords-file-save-on-exit . 'always))
    :config
    (defun my-ebib-name-transform-function (key)
      "Search file of the form SEARCH-DIRS/1ST-AUTHOR/ENTRY-KEY."
      (format "%s/%s"
              (substring key (string-match "[A-Za-z]+" key) (match-end 0))
              (replace-regexp-in-string ":" "" key)))))

(leaf python
  :doc "Python mode"
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook ((python-mode-hook . lsp))
  :custom
  `((python-shell-interpreter . ,(or (executable-find "python3")
                                     (executable-find "python")))))

(leaf yaml-mode
  :ensure t
  :hook ((yaml-mode-hook . lsp)))

;;; Appearance
(leaf *Appearance
  :config
  (setq inhibit-startup-screen t
        frame-title-format "%f")
  (when (display-graphic-p)
    ;; When display-graphic is true, the following are undefined:
    (tool-bar-mode -1)
    (scroll-bar-mode -1))
  (size-indication-mode 1)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)

  (leaf mood-line
    :ensure t
    :custom ((mood-line-show-eol-style . t)
             (mood-line-show-encoding-information . t))
    :config
    (mood-line-mode 1)))

;;; Fonts
(leaf *Fonts
  :doc "Font settings.

You can change Font Size and Font Family interactively by calling
  `my-change-font-size' and `my-change-font-family' respectively.

You can also specify them by local configuration file.  For instance,

(add-hook 'after-init-hook #'(lambda () (setq my-font-size 16
                                              my-font-family \"Cica\")
                                        (my--font-initialize)))
"
  :config
  (defvar my-font-size 10
    "Font size.")
  (defvar my-font-family ""
    "Font family.")

  (defun my--font-initialize ()
    "Initialize font settings.

After setting the variables `my-font-size' and `my-font-family',
run this function.  For instance, add to `after-init-hook' in `local-conf.el'."
    (let* ((fheight (round (* 10 my-font-size))))
      (set-face-attribute 'default nil
                          :family my-font-family
                          :height fheight)
      (message "Font setting...done")))

  (defun my-change-font-size (size)
    "Set the default font size to SIZE."
    (interactive "nFont Size: ")
    (setq my-font-size size)
    (my--font-initialize))

  (defun my-change-font-family ()
    "Set the default font family to FAMILY using consult"
    (interactive)
    (let* ((font (consult--read (font-family-list))))
      (setq my-font-family font)
      (my--font-initialize))))

;;; Keybindings
(leaf *Keybindings
  :config
  (define-key global-map (kbd "C-m") #'newline-and-indent)
  (define-key global-map (kbd "C-c t o") #'other-window)
  (define-key global-map (kbd "C-;") #'comment-line)
  ;; Swap C-h <-> DEL
  (define-key key-translation-map (kbd "C-h") (kbd "DEL"))
  (define-key global-map (kbd "C-x ?") #'help-for-help)

  ;; Keybinds for navigation
  (defhydra hydra-navi
    (:hint nil)
    "
^Navigate^              ^ ^                 ^Action
^^^^^^-----------------------------------------------------------
_f_: foward char        _n_: next line      _s_: search
_F_: foward word        _p_: previous line  _r_: replace
_b_: backward char      _v_: scroll down
_B_: backward word      _V_: scroll up      _k_: kill buffer
_a_: beginning of line  ^ ^
_e_: end of line        ^ ^                 _x_: execute command
"
    ("n" next-line)
    ("p" previous-line)
    ("f" forward-char)
    ("F" forward-word)
    ("b" backward-char)
    ("B" backward-word)
    ("a" beginning-of-line)
    ("e" move-end-of-line)
    ("v" scroll-up-command)
    ("V" scroll-down-command)
    ("s" swiper)
    ("r" query-replace)
    ("x" counsel-M-x)
    ("k" kill-buffer)
    ("q" nil "quit"))
  (define-key global-map (kbd "C-v") #'hydra-navi/body)
  )

(provide 'config)
;;; config.el ends here
