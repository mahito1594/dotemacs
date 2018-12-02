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

(provide '20_tex)
;;; 20_tex.el Ends here
