;;; tex --- settings for LaTeX
;;; Commentary:
;; Use YaTeX: Yet Another TeX mode for Emacs.
;;
;;; Code:

(use-package yatex
  :mode (("\\.tex\\'" . yatex-mode)
         ("\\.sty\\'" . yatex-mode)
         ("\\.ltx\\'" . yatex-mode))
  :init
  ;; YaTeX with RefTeX
  ;; see https://texwiki.texjp.org/?YaTeX#iabc8ab6
  (add-hook 'yatex-mode-hook
            '(lambda ()
               (reftex-mode 1)
               (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
               (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-reigion)
               (define-key reftex-mode-map (concat YaTeX-prefix ")") 'YaTeX-insert-parens-region)))
  :config
  (setq YaTeX-inhibit-prefix-letter t)  ; prefix: C-c => C-c C-
  (setq YaTeX-kanji-code 4) ; use UTF-8
  (setq YaTeX-use-AMS-LaTeX t)
  (setq tex-command "latexmk -lualatex")
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

;; BibTeX
(use-package bibtex
  :ensure nil
  :config
  (setq bibtex-user-optional-fields
        '(("yomi" "Yomigana")
          ("MRNUMBER" "Math. Rev. number")
          ("archivePrefix" "name of preprint server" "arXiv")
          ("eprint" "Electric preprint")
          ("primaryClass" "Primary class used by arXiv")))
  (setq bibtex-autokey-name-case-convert 'capitalize)
  (setq bibtex-autokey-titleword-case-convert 'capitalize)
  (setq bibtex-autokey-titleword-separator "")
  (setq bibtex-autokey-titleword-length nil)
  (setq bibtex-autokey-titlewords 1)
  (setq bibtex-autokey-year-title-separator ":")
  (setq bibtex-autokey-titleword-ignore
        '("A" "An" "On" "The" "a" "an" "on" "the")))

(use-package ebib
  :init
  (setq ebib-bibtex-dialect 'BibTeX)
  ;; (setq ebib-bibtex-dialect 'biblatex)
  (setq ebib-extra-fields
        '((BibTeX "crossref" "annote" "keywords" "doi" "archivePrefix" "eprint" "primaryClass" "MRCLASS" "MRNUMBER")
          (biblatex "crossref" "annotation" "keywords" "archivePrefix" "eprint" "primaryClass" "MRCLASS" "MRNUMBER")))
  :config
  (when (eq system-type 'darwin)
    (setq ebib-bib-search-dirs
          '("~/Library/texmf/bibtex/bib"))
    (setq ebib-preload-bib-files
          '("~/Library/texmf/bibtex/bib/articles.bib"
            "~/Library/texmf/bibtex/bib/books.bib"
            "~/Library/texmf/bibtex/bib/miscs.bib"))))

(provide '20_tex)
;;; 20_tex.el Ends here
