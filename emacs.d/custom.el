(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/backups/emacs.backups")))
 '(c-basic-offset 2)
 '(column-number-mode t)
 '(css-indent-offset 2)
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "82358261c32ebedfee2ca0f87299f74008a2e5ba5c502bde7aaa15db20ee3731" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "9ea054db5cdbd5baa4cda9d373a547435ba88d4e345f4b06f732edbc4f017dc3" "fa189fcf5074d4964f0a53f58d17c7e360bb8f879bd968ec4a56dc36b0013d29" "61a83dbf3d3722d70abee8fb6dbc3566766ff86c098c2a925f2ccfd4d5b3a756" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))
 '(delete-old-versions t)
 '(evil-want-C-u-scroll t)
 '(evil-want-Y-yank-to-eol nil)
 '(frame-background-mode 'dark)
 '(haskell-process-type 'stack-ghci)
 '(haskell-stylish-on-save t t)
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(ido-enable-flex-matching t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(ispell-program-name "aspell")
 '(js-indent-level 2)
 '(js3-enter-indents-newline t)
 '(js3-indent-on-enter-key t)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(line-number-mode t)
 '(lsp-enable-symbol-highlighting nil)
 '(lsp-symbol-highlighting-skip-current nil)
 '(markdown-command "marked --gfm")
 '(package-selected-packages
   '(geiser racket-mode "pinentry" "pinentry" pinentry mu4e-maildirs-extension mu4e-alert helm-mu seeing-is-believing rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocopfmt rubocop rspec-mode robe rbenv rake minitest enh-ruby-mode chruby bundler inf-ruby nord-theme json-navigator hierarchy nodejs-repl import-js grizzl helm-gtags ggtags counsel-gtags counsel swiper ivy add-node-modules-path dap-mode bui tree-mode lsp-ui lsp-treemacs lsp-python-ms python helm-lsp cquery company-lsp ccls lsp-mode yasnippet-snippets writeroom-mode visual-fill-column treemacs-projectile treemacs-evil treemacs ht pfuture symon symbol-overlay string-inflection spaceline-all-the-icons prettier-js pippel pipenv password-generator overseer org-cliplink org-brain nameless magit-svn importmagic epc ctable concurrent impatient-mode helm-xref helm-rtags helm-purpose window-purpose imenu-list helm-org-rifle helm-org helm-git-grep google-c-style gitignore-templates flycheck-rtags flycheck-package package-lint flycheck-bashate evil-textobj-line evil-org transient evil-lion evil-goggles evil-cleverparens paredit editorconfig doom-modeline shrink-path all-the-icons memoize devdocs cpp-auto-include company-rtags rtags centered-cursor-mode browse-at-remote blacken lv font-lock+ dotenv-mode org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download htmlize gnuplot toml-mode racer flycheck-rust cargo rust-mode winum unfill ob-elixir insert-shebang fuzzy flycheck-credo ghub phpunit phpcbf php-extras php-auto-yasnippets drupal-mode php-mode erlang rcirc-notify rcirc-color fish-mode company-shell yaml-mode flycheck-ycmd company-ycmd ycmd request-deferred deferred flycheck-mix alchemist elixir-mode web-mode tagedit smeargle slim-mode scss-mode sass-mode pug-mode orgit org magit-gitflow less-css-mode helm-gitignore helm-css-scss haml-mode gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup emmet-mode company-web web-completion-data yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic xterm-color web-beautify utop tuareg caml shell-pop ocp-indent multi-term merlin livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc intero hlint-refactor hindent helm-hoogle haskell-snippets git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-commit with-editor git-gutter flycheck-pos-tip pos-tip flycheck-haskell flycheck eshell-z eshell-prompt-extras esh-help disaster diff-hl company-tern dash-functional tern company-ghci company-ghc ghc haskell-mode company-cabal company-c-headers coffee-mode cmm-mode cmake-mode clang-format reveal-in-osx-finder pbcopy osx-trash osx-dictionary launchctl ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree mwim move-text mmm-mode markdown-toc markdown-mode macrostep lorem-ipsum linum-relative link-hint info+ indent-guide ido-vertical-mode hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gh-md flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f s diminish define-word company-statistics company column-enforce-mode clean-aindent-mode bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup quelpa package-build spacemacs-theme))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(powerline-default-separator 'slant)
 '(safe-local-variable-values
   '((eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))
     (erlang-indent-level . 4)))
 '(same-window-regexps '("\\*grep\\*" "\\*ack\\*"))
 '(send-mail-function 'mailclient-send-it)
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(sp-autoescape-string-quote nil)
 '(sp-autoskip-closing-pair 'always)
 '(sp-base-key-bindings 'paredit)
 '(tab-width 2)
 '(truncate-lines t)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(vc-follow-symlinks t)
 '(version-control t)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(button ((t (:underline t :box nil))))
 '(font-lock-comment-face ((t (:foreground "#2aa1ae" :background nil :slant normal)))))
