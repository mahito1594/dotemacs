;;; tex --- settings for LaTeX
;;; Commentary:
;; Use YaTeX: Yet Another TeX mode for Emacs.
;;
;;; Code:

(use-package yatex
  :mode (("\\.tex\\'" . yatex-mode)
         ("\\.sty\\'" . yatex-mode)
         ("\\.ltx\\'" . yatex-mode))
  :config
  ;; settings:
  (setq YaTeX-inhibit-prefix-letter t)
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
  ;; my symbols
  (setq YaTeX-math-sign-alist-private
        '(("NN" "setN" "N")
          ("ZZ" "setZ" "Z")
          ("QQ" "setQ" "Q")
          ("RR" "setR" "R")
          ("CC" "setC" "C")
          ("FF" "setF" "F")
          ("Fp" "setF_p" "Fp")
          ("Fq" "setF_q" "Fq")
          ("AA" "setA" "AA")))
  )

;; RefTeX
(use-package reftex
  :ensure nil
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
  ;; automatic insert non-breaking whitespace (~) before citation. See
  ;; https://www.emacswiki.org/emacs/RefTeX
  (setq reftex-format-cite-function
        '(lambda (key fmt)
           (let ((cite (replace-regexp-in-string "%l" key fmt)))
             (if (or (= ?~ (string-to-char fmt))
                     (member (preceding-char) '(?\ ?\t ?\n ?~)))
                 cite
               (concat "~" cite)))))
  ;; source files
  (setq reftex-bibpath-environment-variables
        '("!kpsewhich -show-path=.bib"))
  (setq reftex-bibliography-commands
        '("bibliography" "nobibliography" "addbibresource")))

;; BibTeX
(use-package bibtex
  :ensure nil
  :config
  (setq bibtex-user-optional-fields
        '(("yomi" "Yomigana")
          ("MRNUMBER" "Math. Rev. number")
          ("archivePrefix" "name of preprint server" "arXiv")
          ("eprint" "Electric preprint")
          ("primaryClass" "Primary class used by arXiv")
          ("shortjournal" "Journal Abbreviations")))
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
