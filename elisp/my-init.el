;;; my-init.el --- My configuration file for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2019  TANNO Mahito

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

;; `my-init.el' is my configuration for Emacs.  You can get details in
;; `README.org' or in `.emacs.d/doc/index.html' generated by Org-mode.

;; Do not edit this file directly.  If you want to edit `my-init.el',
;; you must edit `README.org' instead.

;;; Code:

(defconst my-site-lisp-directory
  (expand-file-name "site-lisp/" user-emacs-directory)
  "We should put here packages.")

(let ((default-directory my-site-lisp-directory))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell)
  (delete "-i" exec-path-from-shell-arguments) ; drop "-i" from option
  (exec-path-from-shell-initialize))

(defvar my-backup-directory (expand-file-name "backups/" user-emacs-directory)
  "We save automatically files into the directory:
We set `backup-directory-alist' and `auto-save-file-name-transforms' to `my-backup-directory'.")

(defvar my-local-config-file (expand-file-name "local-conf.el" user-emacs-directory)
  "You put Emacs Lisp files here for local config.")

(setq straight-repository-branch "develop") ; use the develop branch of straight.el
(if (and (executable-find "watchexec")
         (executable-find "python3"))
    (setq straight-check-for-modifications '(watch-files find-when-checking))
  (setq straight-check-for-modifications '(check-on-save find-when-checking)))

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

(setq straight-vc-git-default-clone-depth 1) ; shallow clone

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

(straight-use-package 'org)

(straight-use-package 'flymake)

(use-package blackout
  :straight (:host github :repo "raxod502/blackout")
  :demand t)

(use-package all-the-icons
  :if (window-system)
  :demand t
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

(use-package hydra
  :demand t)

(use-package use-package-hydra
  :demand t
  :after (hydra))

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

(use-package mozc
  :if (eq system-type 'gnu/linux)
  :demand t
  :config
  (setq default-input-method "japanese-mozc"))

(use-feature ucs-normalize
  :if (eq system-type 'darwin)
  :demand t
  :config
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

(use-feature server
  :hook (after-init . server-mode))

(use-package restart-emacs
  :commands (restart-emacs))

(use-package esup
  :commands (esup))

(setq backup-directory-alist
      `((".*" . ,my-backup-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,my-backup-directory t)))
(setq auto-save-list-file-prefix
      (concat my-backup-directory
              "/.saves-"))

(use-feature dired
  :custom
  (dired-recursive-copies 'always)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-feature dired-x
  :hydra
  (hydra-dired
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
   ("." nil))
  :bind (:map dired-mode-map
              ("." . hydra-dired/body))
  :demand t
  :after (dired)
  :custom
  (dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$"))

(use-package all-the-icons-dired
  :if (window-system)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package counsel
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-S-s" . swiper-all)
         ("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("<tab>" . ivy-alt-done)
         ("C-w" . ivy-yank-word))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-wrap t)
  (ivy-format-function 'ivy-format-function-arrow)
  (counsel-yank-pop-separator "\n<--------->\n")
  (ivy-initial-inputs-alist nil)
  :blackout t)

(use-package ivy-hydra
  :bind (:map ivy-minibuffer-map
              ("C-o" . hydra-ivy/body)))

(use-package ivy-rich
  :functions (my-ivy-rich-buffer-icon my-ivy-rich-file-icon)
  :hook (ivy-mode . ivy-rich-mode)
  :custom
  (ivy-rich-path-style 'abbrev)
  (ivy-rich-display-transformers-list
   '(ivy-switch-buffer
     (:columns
      ((my-ivy-rich-buffer-icon :width 2)
       (ivy-rich-candidate (:width 30))
       (ivy-rich-switch-buffer-size (:width 7))
       (ivy-rich-switch-buffer-indicators (:width 4 :face error :align left))
       (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
       (ivy-rich-switch-buffer-project (:width 15 :face success))
       (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
      :predicate
      (lambda (cand) (get-buffer cand)))
     counsel-M-x
     (:columns
      ((counsel-M-x-transformer (:width 40))
       (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
     counsel-describe-function
     (:columns
      ((counsel-describe-function-transformer (:width 40))
       (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
     counsel-describe-variable
     (:columns
      ((counsel-describe-variable-transformer (:width 40))
       (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
     counsel-recentf
     (:columns
      ((ivy-rich-candidate (:width 0.8))
       (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
     counsel-find-file
     (:columns
      ((my-ivy-rich-file-icon :width 2)
       (ivy-rich-candidate)))
     counsel-git
     (:columns
      ((my-ivy-rich-file-icon :width 2)
       (ivy-rich-candidate)))))
  :blackout t)

(use-package amx
  :hook (ivy-mode . amx-mode))

(use-package ivy-ghq
  :straight (:host github :repo "analyticd/ivy-ghq")
  :if (executable-find "ghq")
  :commands (ivy-ghq-open)
  :custom
  (ivy-ghq-short-list nil))

(use-package which-key
  :hook (after-init . which-key-mode)
  :bind (:map which-key-mode-map
              ("C-x DEL" . which-key-C-h-dispatch)
              ("C-c DEL" . which-key-C-h-dispatch))
  :custom
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  :blackout t)

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

(use-package lice)

(use-package magit
  :commands (magit-status)
  :bind ("C-x g" . magit-status))

(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*"
  (emacs-lock-mode 'kill))

(use-feature elec-pair
  :hook (after-init . electric-pair-mode))

(use-feature paren
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-style 'mixed))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(setq-default indent-tabs-mode nil)

(use-feature whitespace
  :commands (whitespace-mode)
  :bind (("C-c w" . whitespace-mode))
  :custom
  (whitespace-style '(
                      face
                      trailing
                      tabs
                      spaces
                      empty
                      space-mark
                      tab-mark
                      ))
  :blackout t)

(use-package beacon
  :hook (after-init . beacon-mode)
  :custom
  (beacon-color "yellow"))

(use-package symbol-overlay
  :hook ((prog-mode . symbol-overlay-mode))
  :bind (("M-i" . symbol-overlay-put)))

(add-hook 'after-init-hook #'transient-mark-mode)

(setq ring-bell-function 'ignore)

(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :blackout t)

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (setq company-require-match 'never)
  :blackout t)

(use-package company-quickhelp
  :if (window-system)
  :hook (company-mode . company-quickhelp-mode))

(use-package company-box
  :functions (all-the-icons-faicon all-the-icons-octicon all-the-icons-material all-the-icons-alltheicon)
  :if (and (window-system)
           (>= emacs-major-version 26))
  :init
  (defvar my-company-box-icons-all-the-icons
    `((Unknown       . ,(all-the-icons-faicon     "cog"                      :height 0.9))
      (Text          . ,(all-the-icons-octicon    "file-text"                :height 0.9))
      (Method        . ,(all-the-icons-faicon     "cube"                     :height 0.9))
      (Function      . ,(all-the-icons-faicon     "cube"                     :height 0.9))
      (Constructor   . ,(all-the-icons-faicon     "cube"                     :height 0.9))
      (Field         . ,(all-the-icons-faicon     "cog"                      :height 0.9))
      (Variable      . ,(all-the-icons-faicon     "cog"                      :height 0.9))
      (Class         . ,(all-the-icons-faicon     "cogs"                     :height 0.9))
      (Interface     . ,(all-the-icons-material   "share"                    :height 0.9))
      (Module        . ,(all-the-icons-alltheicon "less"                     :height 0.9))
      (Property      . ,(all-the-icons-faicon     "wrench"                   :height 0.9))
      (Unit          . ,(all-the-icons-material   "settings_system_daydream" :height 0.9))
      (Value         . ,(all-the-icons-material   "format_align_right"       :height 0.9))
      (Enum          . ,(all-the-icons-material   "content_copy"             :height 0.9))
      (Keyword       . ,(all-the-icons-material   "filter_center_focus"      :height 0.9))
      (Snippet       . ,(all-the-icons-material   "content_paste"            :height 0.9))
      (Color         . ,(all-the-icons-material   "palette"                  :height 0.9))
      (File          . ,(all-the-icons-faicon     "file"                     :height 0.9))
      (Reference     . ,(all-the-icons-material   "collections_bookmark"     :height 0.9))
      (Folder        . ,(all-the-icons-faicon     "folder"                   :height 0.9))
      (EnumMember    . ,(all-the-icons-material   "format_align_right"       :height 0.9))
      (Constant      . ,(all-the-icons-faicon     "square-o"                 :height 0.9))
      (Struct        . ,(all-the-icons-faicon     "cogs"                     :height 0.9))
      (Event         . ,(all-the-icons-faicon     "bolt"                     :height 0.9))
      (Operator      . ,(all-the-icons-material   "control_point"            :height 0.9))
      (TypeParameter . ,(all-the-icons-faicon     "cogs"                     :height 0.9))
      (Template      . ,(all-the-icons-material   "format_align_center"      :height 0.9))
      ))
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-show-single-candidate t)
  (company-box-max-candidates 50)
  :config
  (setq company-box-backends-colors nil)
  (setq company-box-icons-alist 'my-company-box-icons-all-the-icons)
  :blackout t)

(use-package flycheck
  :commands (flycheck-disable-checker)
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package flycheck-popup-tip
  :hook (flycheck-mode . flycheck-popup-tip-mode))

(use-package flymake-diagnostic-at-point
  :commands (flymake-diagnostic-at-point-mode)
  :hook (flymake-mode . flymake-diagnostic-at-point-mode))

(use-package lsp-mode
  :commands (lsp)
  :custom
  (lsp-prefer-flymake t)
  :config)

(use-package company-lsp
  :demand t
  :after (company)
  :config
  (push 'company-lsp company-backends))

(use-package lsp-ui
  :commands (lsp-ui-mode)
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-flycheck-enable nil)
  :blackout t)

(use-package lsp-latex
  :if (executable-find "texlab")
  :straight (:host github :repo "ROCKTAKEY/lsp-latex")
  :demand t
  :after (lsp-mode))

(use-feature org
  :functions (my-org-electric-pair-mode)
  :hook (org-mode . my-org-electric-pair-mode)
  :custom
  (org-startup-indented t)
  (org-fontify-natively t)
  (org-html-htmlize-output-type 'css)
  :config
  (setq org-structure-template-alist (append '(("el" . "src emacs-lisp"))
                                             org-structure-template-alist)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package ox-gfm
  :demand t
  :after (ox))

(use-package htmlize
  :demand t
  :after (ox))

(use-feature outline
  :hydra
  (hydra-outline
   (:hint nil)
   "
^Navigate^                ^Hide^         ^Show^         ^Edit^
^^^^^^^^^^^^----------------------------------------------------------------
_u_: up                   _l_: leaves    _a_: all       _↑_: move up
_n_: next visible         _t_: body      _e_: entry     _↓_: move down
_p_: previous visible     _c_: entry     _k_: branches  _←_: promote
_f_: forward same level   _d_: subtree   _i_: children  _→_: demote
_b_: backward same level  _q_: sublevel  _s_: subtree
^ ^                       _o_: other     ^ ^            _z_: quit
"
   ;; Navigate
   ("u" outline-up-heading)
   ("n" outline-next-visible-heading)
   ("p" outline-previous-visible-heading)
   ("f" outline-forward-same-level)
   ("b" outline-backward-same-level)
   ;; Hide
   ("l" outline-hide-leaves)
   ("t" outline-hide-body)
   ("c" outline-hide-entry)
   ("d" outline-hide-subtree)
   ("q" outline-hide-sublevels)
   ("o" outline-hide-other)
   ;; Show
   ("a" outline-show-all)
   ("e" outline-show-entry)
   ("k" outline-show-branches)
   ("i" outline-show-children)
   ("s" outline-show-subtree)
   ;; Edit
   ("<up>" outline-move-subtree-up)
   ("<down>" outline-move-subtree-down)
   ("<left>" outline-promote)
   ("<right>" outline-demote)
   ;; quit
   ("z" nil))
  :bind (:map outline-minor-mode-map
              ("C-c #" . hydra-outline/body)))

(straight-use-package 'auctex)

(use-feature tex
  :preface
  (defun my-plain-TeX-mode-hook ()
    (outline-minor-mode 1)
    (setq-local TeX-electric-math
                (cons "$" "$")))
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
  :hook ((plain-TeX-mode . my-plain-TeX-mode-hook)
         (TeX-mode . lsp))
  :custom
  (TeX-auto-save nil)
  (TeX-parse-self t)
  (TeX-electric-sub-and-superscript t)
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method '((dvi . synctex)
                                 (pdf . synctex)))
  :config
  (add-to-list 'TeX-command-list
               '("LatexMk" "latexmk %t"
                 TeX-run-TeX nil
                 (latex-mode) :help "Run latexmk")))

(use-feature latex
  :preface
  ;; Below function is due to A. Esbati.  See
  ;; https://tex.stackexchange.com/questions/320524/how-to-deactivate-eqnarray-environment-in-auctex
  ;; licensed under CC-BY-SA 4.0
  (defun my-LaTeX-remove-eqnarray-from-environments ()
    "Remove \"eqnarray\" and \"eqnarray*\" environmens from the variable
`LaTeX-environment-list', they should not be used.  For more detail, see
Madsen's report (\"Avoid eqnarray!\")."
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
                (cons "\\(" "\\)")))
  :hook ((LaTeX-mode . my-LaTeX-mode-hook)
         (LaTeX-mode . lsp)
         (TeX-auto-cleanup . my-LaTeX-remove-eqnarray-from-environments))
  :custom
  (LaTeX-label-alist nil)
  (LaTeX-electric-left-right-brace t)
  :config
  (remove-hook 'LaTeX-section-hook #'LaTeX-section-label))

(use-feature font-latex
  :custom
  (font-latex-fontify-script nil))

(use-feature tex-jp
  :custom
  (japanese-TeX-engine-default 'uptex)
  (japanese-LaTeX-default-style "jsarticle")
  (japanese-LaTeX-style-list
   '(("jsarticle") ("jsreport") ("jsbook")
     ;; for upLaTeX
     ("ujarticle") ("ujreport") ("ujbook")
     ("utarticle") ("utreport") ("utbook")
     ;; for LuaLaTeX
     ("ltjarticle") ("ltjreport") ("ltjbook")
     ("ltjsarticle") ("ltjsreport") ("ltjsbook")
     ;; for XeLaTeX/LuaTeX
     ("bxjsarticle") ("bxjsreport") ("bxjsbook") ("bxjsslide")
     ;; for jlreq
     ("jlreq")))
  :config
  ;; By setting `TeX-expand-list', override `TeX-expand-list-builtin'
  ;; which is modified by `tex-jp.el'.
  (setq TeX-expand-list
        (append TeX-expand-list
                '(("%(bibtex)" (lambda ()
                                 (cond
                                  ((eq TeX-engine 'ptex)
                                   (if (executable-find "pbibtex")
                                       "pbibtex %(kanjiopt)" "jbibtex"))
                                  ((eq TeX-engine 'jtex) "jbibtex")
                                  ((and japanese-TeX-mode
                                        (memq TeX-engine '(uptex xetex luatex)))
                                   "upbibtex")
                                  (t "bibtex")))))))
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
  (advice-add 'japanese-LaTeX-guess-engine :override #'my-japanese-LaTeX-guess-engine))

(use-package company-math
  :if (not (executable-find "texlab"))
  :demand t
  :after (company)
  :config
  (defun my-LaTeX-mode-setup ()
    (setq-local company-backends
                (append '((company-math-symbols-latex company-latex-commands))
                        company-backends)))
  (add-hook 'LaTeX-mode-hook #'my-LaTeX-mode-setup))

(use-feature reftex
  :hook (LaTeX-mode . reftex-mode)
  ;; :bind (:map reftex-mode-map
              ;; ("C-c )" . nil)
              ;; ("C-c (" . reftex-reference)
              ;; ("C-c {" . reftex-cleveref-cref))
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-ref-style-default-list '("Cleveref"))
  (reftex-label-alist '((nil ?e nil "~\\ref{%s}" nil nil) ; omit parens surrounding eq-like reference
                        ("definition"  ?d "def:"  "~\\ref{%s}" nil ("definiton")   nil)
                        ("proposition" ?p "prop:" "~\\ref{%s}" nil ("proposition") nil)
                        ("theorem"     ?p "thm:"  "~\\ref{%s}" nil ("theorem")     nil)
                        ("lemma"       ?p "lem:"  "~\\ref{%s}" nil ("lemma")       nil)
                        ("corollary"   ?p "cor:"  "~\\ref{%s}" nil ("corollary")   nil)
                        ("remark"      ?r "rem:"  "~\\ref{%s}" nil ("remark")      nil)
                        ("example"     ?x "ex:"   "~\\ref{%s}" nil ("example")     nil)
                        ("conjecture"  ?c "conj:" "~\\ref{%s}" nil ("conjecture")  nil)))
  (reftex-use-external-file-finders t)
  (reftex-external-file-finders '(("tex" . "kpsewhich -format=.tex %f")
                                  ("bib" . "kpsewhich -format=.bib %f")))
  (reftex-bibliography-commands '("bibliography"
                                  "nobibliography"
                                  "addbibresource")))

(use-feature bibtex
  :mode (("\\.bib\\'" . bibtex-mode))
  :bind (:map bibtex-mode-map
              ("C-j" . nil)
              ("C-<return>" . bibtex-next-field))
  :custom
  (bibtex-user-optional-fields '(("yomi" "Yomigana")
                                 ("MRNUMBER" "Math. Rev. Number")
                                 ("archivePrefix" "name of preprint server" "arXiv")
                                 ("eprint" "Electric Print")
                                 ("primaryClass" "Primary class used by arXiv")
                                 ("shortjournal" "Journal Abbreviation")))
  (bibtex-autokey-name-case-convert-function 'capitalize)
  (bibtex-autokey-titleword-case-convert-function 'capitalize)
  (bibtex-autokey-titleword-separator "")
  (bibtex-autokey-titleword-length nil)
  (bibtex-autokey-titlewords 1)
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-year-title-separator ":")
  (bibtex-autokey-titleword-ignore '("A" "An" "On" "The" "a" "an" "on" "the"
                                     "Le" "La" "Les" "le" "la" "les"
                                     "Zur" "zur")))

(use-package ebib
  :functions (my-ebib-name-transform-function)
  :preface
  (defvar my-ebib-keywords-file (expand-file-name "~/texmf/emacs/ebib-keywords.txt")
    "You put here `ebib-keywords.txt'.")
  :commands (ebib)
  :bind (:map ebib-multiline-mode-map
              ("C-c C-c" . ebib-quit-multiline-buffer-and-save))
  :custom
  (ebib-bitex-dialect 'BibTeX)
  ;; Preload database
  (ebib-preload-bib-files '("~/texmf/bibtex/bib/articles.bib"
                            "~/texmf/bibtex/bib/books.bib"
                            "~/texmf/bibtex/bib/others.bib"))
  ;; Extra fields
  (ebib-extra-fields '((BibTeX "crossref"
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
  ;; Files
  (ebib-file-search-dirs '("~/BibFile/Papers"
                           "~/BibFile/Books"
                           "~/BibFile/Proceedings"))
  (ebib-name-transform-function #'my-ebib-name-transform-function)
  (ebib-file-associations (cond ((eq system-type 'darwin) '(("pdf" . "open") ("ps" . "open")))
                                (t '(("pdf" . "xpdf") ("ps" . "gv")))))
  ;; Keywords
  (ebib-keywords-use-only-file t)
  (ebib-keywords-field-keep-sorted t)
  (ebib-keywords-file-save-on-exit 'always)
  (ebib-keywords-file my-ebib-keywords-file))

(use-package markdown-mode
  :defines (my-markdown-electric-pair-pairs)
  :functions (my-markdown-electric-pair-mode)
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :hook ((gfm-mode markdown-mode) . my-markdown-electric-pair-mode)
  :bind (("C-c C-," . markdown-insert-gfm-code-block))
  :init
  (setq markdown-command "marked")
  :custom
  (markdown-fontify-code-blocks-natively t))

(use-package edit-indirect)

(use-package ccls
  :hook ((c-mode c++-mode objc-mode) . (lambda ()
                                         (require 'ccls)
                                         (lsp)))
  :config
  (setq ccls-sem-highlight-method 'font-lock))

(use-package modern-cpp-font-lock
  :commands (modern-c++-font-lock-mode)
  :hook (c++-mode-hook . modern-c++-font-lock-mode)
  :blackout t)

(use-feature elisp-mode
  :blackout (lisp-interaction-mode . "Lisp-Interaction"))

(use-package js2-mode
  :mode "\\.js\\'")

(use-package tuareg
  :hook (tuareg-mode . lsp))

(use-feature python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook (python-mode . (lambda ()
                         (lsp)
                         (setq-local indent-tabs-mode nil)
                         (setq-local tab-width 4)))
  :config
  (when (executable-find "python3")
    ;; use python3 if it exists
    (setq python-shell-interpreter "python3")))

(use-package csv-mode)

(use-package yaml-mode)

(use-package dockerfile-mode
  :mode ("Dockerfile\'" . dockerfile-mode))

(use-package kaolin-themes
  :demand t
  :config
  (load-theme 'kaolin-aurora t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (find-file-visit-truename t)
  :config
  (setq doom-modeline-mu4e nil)
  (setq doom-modeline-irc nil)
  (setq column-number-mode t))

(setq inhibit-startup-screen t)
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))
(size-indication-mode +1)
(setq frame-title-format "%f")

(when (version<= "26.0.50" emacs-version)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))

(set-frame-parameter nil 'fullscreen 'maximized)

(define-key global-map (kbd "C-m") #'newline-and-indent)
(define-key global-map (kbd "C-2") #'set-mark-command)
(define-key global-map (kbd "C-t") #'other-window)
(define-key global-map (kbd "C-;") #'comment-line)
(define-key global-map (kbd "C-v") #'hydra-navi/body)

(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(define-key global-map (kbd "C-x ?") 'help-for-help)

(setq custom-file my-local-config-file)
(load my-local-config-file t)

(provide 'my-init)
;;; my-init.el ends here
