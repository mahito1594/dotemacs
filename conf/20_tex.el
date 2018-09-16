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
  (setq YaTeX-kanji-code 4) ; use UTF-8
  (setq YaTeX-use-AMS-LaTeX t)
  (setq tex-command "latexmk -lualatex")
  ;; my symbols
  (setq YaTeX-math-sign-alist-private
        '(("NN" "setN")
          ("ZZ" "setZ")
          ("QQ" "setQ")
          ("RR" "setR")
          ("CC" "setC")
          ("FF" "setF")
          ("Fp" "setF_p")
          ("Fq" "setF_q")))
  )

(provide '20_tex)
;;; 20_tex.el Ends here
