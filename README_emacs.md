# .emcs.d
init-loader と use-packege を利用し `.emacs.d/conf/` 以下の設定ファイルを読み込む．
分割したファイルは概ね以下に従って番号をふる．

* 00 -- 09: init Emacs
* 10 -- 19: fundamental setting for programinngs
* 20 -- 89: settings for indivisual language
* 90 -- 98: computer algebra systems
* 99: keybindings

現在の構成は以下の通り：

```
conf/
├── 01_util.el
├── 02_display.el
├── 09_font.el
├── 10_helm.el
├── 11_exec-path-from-shell.el
├── 12_auto-complete.el
├── 13_flycheck.el
├── 14_tree.el
├── 20_tex.el
├── 31_python-mode.el
├── 90_macaulay2.el
├── 91_singular.el
├── 92_sage.el
└── 99_keybind.el
``` 
## [NeoTree](https://github.com/jaypei/emacs-neotree)
tree の表示に all-the-icons を利用している．
`M-x all-the-icons-install-fonts` とした後，ターミナルで `fc-cache -fv` を実行する．

## Python
Python の記述には python-mode.el を使う．
補完には auto-complete.el と [jedi](https://pypi.org/project/jedi/) を利用する．

jedi と virtualenv を pip 等でインストールした後

```
M-x package-install RET jedi RET
M-x jedi:install-server RET
```

とする．

## Untracking files
以下のファイルは git 管理外．
環境に応じて適当に書き加えること．

### 19_font.el
フォントに関する設定．
例えば

```elisp
(set-face-attribute 'default nil
                    :family "Source Han Code JP"
                    :height 140)
```

 などとする．

### 91_singular.el
SINGULAR を Emacs で動かすための設定．

```elisp
(add-to-list 'load-path "<singular-emacs-home-directory>")
(autoload 'singular "singular"
  "Start Singular using default values." t)
(autoload 'singular-other "singular"
  "Ask for arguments and start Singular." t)
```

詳細は [Running SINGULAR under Emacs](https://www.singular.uni-kl.de/Manual/latest/sing_23.htm#SEC30) を参照のこと．

#### 92_sage.el
SageMath を Emacs で動かすための設定．
使い方は [sage-shell-mode](https://github.com/sagemath/sage-shell-mode) を参照．
`path/to/sage/root_directory` は Sage terminal 上で `import os; print os.environ["SAGE_ROOT"]` と打つことで調べることができる．

```elisp
(use-package sage-shell-mode
  :hook ((sage-shell-mode . eldoc-mode)
         (sage-shell:sage-mode . eldoc-mode))
  :config
  (setq sage-shell:sage-root "path/to/sage/root_directory")
  (with-eval-after-load 'sage-shell-mode
    (sage-shell:define-alias))
  (use-package auto-complete-sage
    :init
    (add-hook 'sage-shell:sage-shell-mode-hook 'ac-sage-setup)
    (add-hook 'sage-shell-mode-hook 'ac-sage-setup)
    :config
    (setq sage-shell:completion-function 'completion-at-point)
    (setq ac-sage-show-quick-help t)
    (setq ac-sage-complete-on-dot t))
  (use-package helm-sage
    :config
    (eval-after-load 'sage-shell-mode
      '(sage-shell:defin-keys sage-shell-mode-map
                              "C-c C-i" 'helm-sage-complete
                              "C-c C-h" 'helm-sage-describe-object-at-point
                              "M-r"     'helm-sage-command-history
                              "C-c o"   'helm-sage-output-history))))
```

