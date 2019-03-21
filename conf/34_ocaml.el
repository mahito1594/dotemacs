;;; ocaml --- settigs for ocaml

;; This program is released under the GPL v3.0 or,
;; (at your option) any later version.  See LICENSE.

;;; Commentary:

;; To use LSP, we need `merlin' of develop version, see
;; https://khady.info/emacs-ocaml-lsp.html

;; So, we use tuareg and merlin.
;; We need install merlin, by running
;; `opam install merlin', and then
;; `opam user-setup install'.

;;; Code:

(use-package tuareg
  :custom
  (merlin-error-after-save nil))

(use-package merlin
  :hook ((caml-mode tuareg-mode) . merlin-mode)
  :config
  (require 'opam-user-setup (locate-user-emacs-file "opam-user-setup.el"))
  (add-to-list 'company-backends 'merlin-company-backend)
  (use-package merlin-eldoc
    :hook ((caml-mode tuareg-mode) . merlin-eldoc-setup))
  (use-package flycheck-ocaml
    :demand t
    :after (flycheck)
    :config
    (flycheck-ocaml-setup)))

(provide '34_ocaml)
;;; 34_ocaml.el ends here
