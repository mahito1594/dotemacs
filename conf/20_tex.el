;;; tex --- settings for LaTeX

;; This program is released under the GPL v3.0 or,
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; Use YaTeX (Yet Another TeX mode for Emacs) for writing
;; LaTeX documents with RefTeX, outline-minor-mode.

;; In order to manage BibTeX database, we use Ebib, see
;; http://joostkremers.github.io/ebib/

;;; Code:

(use-package yatex
  :mode (("\\.tex\\'" . yatex-mode)
         ("\\.sty\\'" . yatex-mode)
         ("\\.ltx\\'" . yatex-mode))
  :init
  (setq YaTeX-inhibit-prefix-letter t)
  :config
  (setq YaTeX-kanji-code 4) ; use UTF-8
  (setq YaTeX-use-AMS-LaTeX t)
  (setq tex-command "latexmk")
  (add-hook 'align-load-hook
            '(lambda ()
               (add-to-list 'align-rules-list
                            '(yatex-table
                              (regexp . "\\(\\s-*\\)&")
                              (repeat . t)
                              (mode . '(yatex-mode))))))
  (setq YaTeX-math-sign-alist-private
        ;; my math symbols
        '(("NN" "setN" "N")
          ("ZZ" "setZ" "Z")
          ("QQ" "setQ" "Q")
          ("RR" "setR" "R")
          ("CC" "setC" "C")
          ("FF" "setF" "F")
          ("Fp" "setF_p" "Fp")
          ("Fq" "setF_q" "Fq")
          ("AA" "setA" "AA")))
  ;; Completion
  (use-package company-math
    ;; completion by company
    :demand t
    :config
    (add-to-list 'company-backends 'company-math-symbols-latex)
    (add-to-list 'company-backends 'company-latex-commands))
  ;; Syntax checker
  (use-package flycheck-yatex
    :straight (:host github :repo "mahito1594/flycheck-yatex")
    :demand t)
  ;; with outline-minor-mode:
  ;; The following code is a modification a part of `tex-mode.el'
  ;; which is bundled with GNU Emacs.
  ;; Copyright (C) 1985-1986, 1989, 1992, 1994-1999, 2001-2019 Free
  ;; Software Foundation, Inc.
  ;; Released under the GPL v3.0 or any later version.
  (defvar my/YaTeX-section-alist
    '(("part" . 0)
      ("chapter" . 1)
      ("section" . 2)
      ("subsection" . 3)
      ("subsubsection" . 4)
      ("paragraph" . 5)))
  (defvar my/YaTeX-metasection-list
    '("documentclass"
      "begin{document}" "end{document}"
      "frontmatter" "mainmatter" "appendix" "backmatter"))
  (defvar my/YaTeX-outline-regexp
    (concat (regexp-quote "\\")
            (regexp-opt (append my/YaTeX-metasection-list
                                (mapcar #'car my/YaTeX-section-alist))
                        t)))
  (defvar my/YaTeX-outline-promotion-headings
    '("\\chapter" "\\section" "\\subsection" "\\subsubsection"))
  (defun my/YaTeX-outline-level ()
    (if (looking-at my/YaTeX-outline-regexp)
        (1+ (or (cdr (assoc (match-string 1) my/YaTeX-section-alist)) -1))
      1000))
  (defun my/YaTeX-with-outline ()
    (outline-minor-mode 1)
    (setq-local outline-regexp my/YaTeX-outline-regexp)
    (setq-local outline-level #'my/YaTeX-outline-level)
    (setq-local outline-promotion-headings my/YaTeX-outline-promotion-headings))
  (add-hook 'yatex-mode-hook #'my/YaTeX-with-outline))

;; RefTeX
(use-package reftex
  :straight nil
  :hook (yatex-mode . reftex-mode)
  :bind (:map reftex-mode-map
              ("C-c )" . nil)
              ("C-c (" . reftex-reference)
              ("C-c {" . reftex-cleveref-cref))
  :config
  ;; use cleveref in RefTeX, there are two following ways:
  ;; 1. use reftex-cleveref-cref function, OR
  ;; 2. use reftex-reference function
  ;;    with setting the variable reftex-ref-style-default-list to Cleveref
  (setq reftex-ref-style-default-list '("Cleveref"))
  ;; Theorem environments
  (setq reftex-label-alist
        '((nil ?e nil "~\\ref{%s}" nil nil) ; omit parens surrounding eq-like reference
          ("definition"  ?d "def:"  "~\\ref{%s}" nil ("definiton")   nil)
          ("proposition" ?p "prop:" "~\\ref{%s}" nil ("proposition") nil)
          ("theorem"     ?p "thm:"  "~\\ref{%s}" nil ("theorem")     nil)
          ("lemma"       ?p "lem:"  "~\\ref{%s}" nil ("lemma")       nil)
          ("corollary"   ?p "cor:"  "~\\ref{%s}" nil ("corollary")   nil)
          ("remark"      ?r "rem:"  "~\\ref{%s}" nil ("remark")      nil)
          ("example"     ?x "ex:"   "~\\ref{%s}" nil ("example")     nil)
          ("conjecture"  ?c "conj:" "~\\ref{%s}" nil ("conjecture")  nil)))
  ;; source files
  (setq reftex-bibpath-environment-variables
        '("!kpsewhich -show-path=.bib"))
  (setq reftex-bibliography-commands
        '("bibliography" "nobibliography" "addbibresource"))
  :blackout t)

;; BibTeX
(use-package bibtex
  :straight nil
  :mode (("\\.bib\\'" . bibtex-mode))
  :bind (:map bibtex-mode-map
              ("C-j" . nil)
              ("<C-return>" . bibtex-next-field))
  :config
  ;; Add optional fields
  (setq bibtex-user-optional-fieldsa
        '(("yomi" "Yomigana")
          ("MRNUMBER" "Math. Rev. number")
          ("archivePrefix" "name of preprint server" "arXiv")
          ("eprint" "Electric preprint")
          ("primaryClass" "Primary class used by arXiv")
          ("shortjournal" "Journal Abbreviations")))
  ;; Customize `bibtex-generate-autokey' function
  (setq bibtex-autokey-name-case-convert 'capitalize)
  (setq bibtex-autokey-titleword-case-convert 'capitalize)
  (setq bibtex-autokey-titleword-separator "")
  (setq bibtex-autokey-titleword-length nil)
  (setq bibtex-autokey-titlewords 1)
  (setq bibtex-autokey-year-length 4)
  (setq bibtex-autokey-year-title-separator ":")
  (setq bibtex-autokey-titleword-ignore
        '("A" "An" "On" "The" "a" "an" "on" "the"
          "Le" "La" "Les" "le" "la" "les"
          "Zur" "zur")))

(use-package ebib
  :commands (ebib)
  :bind (:map ebib-multiline-mode-map
         ("C-c C-c" . ebib-quit-multiline-buffer-and-save))
  :init
  (setq ebib-bibtex-dialect 'BibTeX) ; use BibTeX as default
  ;; (setq ebib-bibtex-dialect 'biblatex) ; use biblatex as default
  ;; extra fields
  (setq ebib-extra-fields
        '((BibTeX "crossref" "annote" "keywords" "doi" "shortjournal" "archivePrefix" "eprint" "primaryClass" "MRCLASS" "MRNUMBER" "file")
          ;; in biblatex, the following fields are treated as alias:
          ;; journal       => journaltitle
          ;; annote        => annotation
          ;; archivePrefix => eprinttype
          ;; primaryclass  => epritclass
          (biblatex "crossref" "annotation" "keywords" "shortjournal" "archivePrefix" "primaryClass" "MRCLASS" "MRNUMBER" "file")))
  ;; browse file
  (setq ebib-file-search-dirs '("~/BibFile/Papers" "~/BibFile/Books" "~/BibFile/Proceedings"))
  (defun my/ebib-name-transform-function (key)
    "Serach file of the form
       SEARCH-DIRS/FIRST-AUTHOR/ENTRY-KEY"
    (format "%s/%s"
            (substring key (string-match "[A-Za-z]+" key) (match-end 0))
            (replace-regexp-in-string ":" "" key)))
  (setq ebib-name-transform-function #'my/ebib-name-transform-function)
  ;; open PDF/PS with `open' command
  (when (eq system-type 'darwin)
    (setq ebib-file-associations
          '(("pdf" . "open")
            ("ps" . "open"))))
  ;; keywords
  (setq ebib-keywords-field-keep-sorted t)
  (when (eq system-type 'darwin)
    (setq ebib-keywords-file "~/ebib-keywords.txt"))
  (setq ebib-keywords-use-only-file t)
  (setq ebib-keywords-file-save-on-exit 'always)
  :config
  (when (eq system-type 'darwin)
    (setq ebib-bib-search-dirs
          '("~/Library/texmf/bibtex/bib"))
    (setq ebib-preload-bib-files
          '("~/Library/texmf/bibtex/bib/articles.bib"
            "~/Library/texmf/bibtex/bib/books.bib"
            "~/Library/texmf/bibtex/bib/others.bib"))))

(provide '20_tex)
;;; 20_tex.el Ends here
