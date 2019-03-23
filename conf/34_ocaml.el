;;; ocaml --- settigs for ocaml

;; This program is released under the GPL v3.0 or,
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; To use LSP, you should run
;;   npm install -g ocaml-language-server
;;   opam install merlin

;; The devlop version of merlin integrated with LSP.  See
;; https://khady.info/emacs-ocaml-lsp.html

;;; Code:

(use-package tuareg
  :hook (tuareg-mode . lsp)
  )

(provide '34_ocaml)
;;; 34_ocaml.el ends here
