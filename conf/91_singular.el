;;; Singular
;; !! untrack by git !!
(add-to-list 'load-path "/usr/local/Cellar/singular/4.1.1_5/share/singular/emacs")
(autoload 'singular "singular"
  "Start Singular using default values." t)
(autoload 'singular-other "singular"
  "Ask for arguments and start Singular" t)

(provide '91_singular)
;;; 91_singular.el ends here
