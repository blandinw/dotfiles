;; system
(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/evil-paredit")

(defalias 'vi 'evil-mode)

;; pre
(menu-bar-mode -1)
(tool-bar-mode -1)
(linum-mode -1)
(global-whitespace-mode t)

(setq-default line-number-mode t
              column-number-mode t
              tab-width 2
              c-basic-offset 2
              indent-tabs-mode nil)

(setq system-uses-terminfo nil
      backup-by-copying t ;; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/backups/emacs.backups")) ;; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t ;; use versioned backups
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                        kill-buffer-query-functions)
      evil-want-C-u-scroll t)

;; custom functions and macros
(defun add-to-list* (the-list elems)
  (if elems
      (progn
        (add-to-list the-list (car elems))
        (add-to-list* the-list (cdr elems)))
    (eval the-list)))

(defun to-markdown ()
  (interactive)
  (shell-command (concat "marked --gfm " buffer-file-name " | browser")))

(defun wly/switch-to-prev-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

(defun file-string (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun wly/switch-to-or-open-shell ()
  (interactive)
  (let ((buf (find-buffer-visiting "*terminal*")))
    (if buf
        (switch-to-buffer-other-window buf)
      (term "zsh"))))

(defvar loaded-theme)
(defun wly/load-theme (theme-name)
  (load-theme theme-name t)
  (setq loaded-theme theme-name))

(defun ensure-package (package-name)
  (unless (package-installed-p package-name)
    (package-refresh-contents)
    (package-install package-name)))

(defun ensure-packages (packages)
  (if packages
      (progn
        (ensure-package (car packages))
        (ensure-packages (cdr packages)))
    nil))

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(ensure-packages '(ack
                   auto-complete
                   clojure-mode
                   clojure-test-mode
                   cider
                   coffee-mode
                   elixir-mode
                   erlang
                   evil
                   find-file-in-project
                   flycheck
                   haskell-mode
                   ido-ubiquitous
                   js3
                   key-chord
                   linum
                   markdown-mode
                   nginx-mode
                   paredit
                   smartparens
                   rainbow-delimiters
                   ruby-end
                   scss-mode
                   undo-tree
                   emmet-mode))

(require 'evil)
(require 'evil-paredit)
(require 'auto-complete)
(require 'cider)
(require 'find-file-in-project)
(require 'uniquify)
(require 'smartparens-config)
(require 'yasnippet)
(require 'css-mode)
(require 'scss-mode)
;; (require 'sh-mode)

;; xterm compat
(define-key input-decode-map "\e3"     "#")
(define-key input-decode-map "\e[18~"  (kbd "<C-S-backspace>"))
(define-key input-decode-map "\e[9;97" (kbd "C-;"))
(define-key input-decode-map "\e[9;98" (kbd "C-S-("))
(define-key input-decode-map "\e[8;8L" (kbd "C-S-L"))
(define-key input-decode-map "\eOA"    (kbd "<up>"))
(define-key input-decode-map "\eOB"    (kbd "<down>"))
(define-key input-decode-map "\eOC"    (kbd "<right>"))
(define-key input-decode-map "\eOD"    (kbd "<left>"))
(define-key input-decode-map "\e[A"    (kbd "<C-up>"))
(define-key input-decode-map "\e[B"    (kbd "<C-down>"))
(define-key input-decode-map "\e[C"    (kbd "<C-right>"))
(define-key input-decode-map "\e[D"    (kbd "<C-left>"))
(define-key input-decode-map "\e[1;7A" (kbd "<M-up>"))
(define-key input-decode-map "\e[1;7B" (kbd "<M-down>"))

;; javascript
(defun js-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (smartparens-mode 1)
  (auto-complete-mode t))

;; scss
(defun css-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (setq css-indent-offset 2
        tab-width 2)
  (smartparens-mode 1)
  (emmet-mode t)
  (flycheck-mode -1))

(defun scss-hook ()
  (css-hook)
  (setq scss-compile-at-save nil))

(defvar sh-basic-offset)
(defvar sh-indentation)
(defun shell-hook ()
  (smartparens-mode 1)
  (setq tab-width 2
        sh-basic-offset 2
        sh-indentation 2))

(defun html-hook ()
  (smartparens-mode 1)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (emmet-mode t)
  (flycheck-mode -1))

(defun cc-hook ()
  (smartparens-mode 1)
  (local-set-key (kbd "C-c C-k") 'compile)
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun go-hook ()
  (smartparens-mode 1)
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun rust-hook ()
  (smartparens-mode 1)
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun coffee-hook ()
  (smartparens-mode 1)
  (setq tab-width 2))

(defun ruby-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (smartparens-mode 1))

(defun sgml-hook ()
  (smartparens-mode 1)
  (emmet-mode t))

(defun clojure-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)

  (paredit-mode 1)
  (evil-paredit-mode 1)
  (rainbow-delimiters-mode 1)

  (define-clojure-indent
    (defprotocol 'defun)
    ;; compojure
    (GET 'defun)
    (POST 'defun)
    (PUT 'defun)
    (DELETE 'defun)
    (defroutes 'defun)
    (context 'defun)
    (against-background 'defun)
    (background 'defun)
    (fact 'defun)
    (facts 'defun)
    (fnk 'defun)
    (match 'defun)
    (delta 'defun)
    (defcontroller 'defun)
    (nlp 'defun)
    (defsm 'defun)
    (doarr 'defun)
    (if-not-let 'defun)
    (try-nil 'defun)
    ;; httpcheck
    (with 'defun)
    (checking 'defun)
    ;; riemann
    (where 'defun)
    (expired 'defun)
    (streams 'defun)
    ;; CLJS
    (.service 'defun)
    (this-as 'defun)
    ;; core.async
    (go-try 'defun)
    ;; clojure.test.check
    (for-all 'defun)
    )

  ;; fancy
  (font-lock-add-keywords
   'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "λ")
                              nil)))))

  (font-lock-add-keywords
   'clojure-mode `(("\\(#\\)("
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "λ")
                              nil)))))

  (font-lock-add-keywords
   'clojure-mode `(("\\(#\\){"
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "∈")
                              nil))))))

(defun clojurescript-hook ()
  (setq inferior-lisp-program (expand-file-name "script/browser-repl"
                                                (getenv "CLOJURESCRIPT_HOME"))))



(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(defun elisp-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)

  (rainbow-delimiters-mode 1)

  (paredit-mode 1)
  (evil-paredit-mode 1)

  ;; fancy
  (font-lock-add-keywords
   'emacs-lisp-mode `(("(\\(lambda\\)[\[[:space:]]"
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "λ")
                              nil))))))

(defun cider-hook ()
  (cider-turn-on-eldoc-mode))

(eval-after-load 'cider
  '(progn (setq cider-repl-pop-to-buffer-on-connect nil
                cider-popup-stacktraces t
                cider-repl-popup-stacktraces t
                cider-auto-select-error-buffer t
                cider-repl-wrap-history t
                cider-repl-history-size 1000
                cider-repl-history-file "/tmp/cider-repl-history")))

(defun cider-repl-hook ()
  (rainbow-delimiters-mode 1)
  (paredit-mode 1)
  (evil-paredit-mode 1))

(defun cider-popup-buffer-hook ()
  (evil-local-set-key 'normal (kbd "q") 'cider-popup-buffer-quit-function))

(defun elixir-hook ()
  (smartparens-mode 1)
  (flycheck-mode -1)

  ;; end
  (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
       "\\(?:^\\|\\s-+\\)\\(?:do\\)")
  (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
  (ruby-end-mode t))

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(defun haskell-hook ()
  (smartparens-mode 1)
  (turn-on-haskell-indentation)
  (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (ghc-init)
  (message "haskell mode loaded."))

(add-to-list* 'ffip-patterns '("Vagrantfile"
                               "Dockerfile"
                               "*.coffee"
                               "*.cljs"
                               "*.edn"
                               "*.scss"
                               "*.jst"
                               "*.jsx"
                               "*.go"
                               "*.json"
                               "*.java"
                               "*.yml"
                               "*.j2"
                               "*.cfg"
                               "*.markdown"
                               "*.properties"
                               "*.md"
                               "*.rs"
                               "*.ex"
                               "*.exs"))

(defun build-find-excludes (patterns)
  (mapconcat #'(lambda (pat)
                (format "-not -regex \"%s\"" pat))
             patterns
             " "))

(add-to-list* 'ffip-prune-patterns '("cabal-dev"
                                     ".cabal-sandbox"
                                     ".tmp"
                                     "dist"
                                     "node_modules"))
(setq ffip-find-options (build-find-excludes '(;;".*.git.*"
                                               ))
      ffip-full-paths t
      ffip-limit 1024)

;; -----------------------------------------------------------------------------
;; Evil bindings

(evil-mode t)
(key-chord-mode t)
(evil-ex-define-cmd "W" 'evil-write)
(key-chord-define evil-insert-state-map (kbd "jk") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "<enter>") 'evil-ret-and-indent)
(define-key evil-normal-state-map (kbd "-") 'evil-window-next)
(define-key evil-normal-state-map (kbd "_") 'evil-window-prev)
(define-key evil-normal-state-map "\\`" 'evil-buffer)
(define-key evil-normal-state-map "\\b" 'ido-switch-buffer)
(define-key evil-normal-state-map "\\p" 'ffip)
(define-key evil-normal-state-map "\\s" 'wly/switch-to-or-open-shell)
(define-key evil-normal-state-map "\\ve" (lambda ()
                                          (interactive)
                                          (find-file "~/.emacs.d/willy.el")))
(define-key evil-normal-state-map "\\z" 'evil-emacs-state)
(define-key evil-normal-state-map "\\m" 'to-markdown)
(define-key evil-normal-state-map (kbd "C-z") 'suspend-frame)
(define-key evil-visual-state-map "gc" 'comment-or-uncomment-region)
(define-key evil-normal-state-map "gcc" '(lambda ()
                                           (interactive)
                                           (comment-or-uncomment-region (line-beginning-position)
                                                                        (line-end-position))))
(define-key evil-normal-state-map (kbd "M-.") 'cider-jump)

(define-key evil-normal-state-map "\\t" '(lambda ()
                                           (interactive)
                                           (wly/load-theme (if (eq 'solarized-dark loaded-theme)
                                                               'solarized-light
                                                             'solarized-dark))))

;; -----------------------------------------------------------------------------
;; Stuff

(wly/load-theme 'solarized-light)
(setq whitespace-action '(auto-cleanup)
      whitespace-style '(trailing space-before-tab space-after-tab indentation empty lines-tail))

(setq ido-enable-flex-matching t)
(ido-mode t)
(yas-global-mode 1)
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ack-default-directory-function '(lambda (&rest args)
                                        (ffip-project-root)))
(global-set-key (kbd "RET") 'newline-and-indent)

;; -----------------------------------------------------------------------------
;; Smartparens

(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "M-("))

;; -----------------------------------------------------------------------------
;; Autocomplete

(global-auto-complete-mode t)
(add-to-list* 'ac-modes '(coffee-mode
                          scss-mode
                          cider-mode
                          cider-repl-mode
                          rust-mode
                          haskell-mode
                          elixir-mode))

;; (add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-save-hook 'byte-compile-current-buffer)
(add-hook 'emacs-lisp-mode-hook 'elisp-hook)
(add-hook 'clojure-mode-hook 'clojure-hook)
(add-hook 'clojurescript-mode-hook 'clojurescript-hook)
(add-hook 'cider-mode-hook 'cider-hook)
(add-hook 'cider-repl-mode-hook 'cider-repl-hook)
(add-hook 'cider-popup-buffer-mode-hook 'cider-popup-buffer-hook)
(add-hook 'elixir-mode-hook 'elixir-hook)
(add-hook 'haskell-mode-hook 'haskell-hook)
(add-hook 'sgml-mode-hook 'sgml-hook) ;; Auto-starts on any markup modes
(add-hook 'ruby-mode-hook 'ruby-hook)
(add-hook 'coffee-mode-hook 'coffee-hook)
(add-hook 'javascript-mode-hook 'js-hook)
(add-hook 'js3-mode-hook 'js-hook)
(add-hook 'css-mode-hook 'css-hook)
(add-hook 'scss-mode-hook 'scss-hook)
(add-hook 'html-mode-hook 'html-hook)
(add-hook 'sh-mode-hook 'shell-hook)
(add-hook 'web-mode-hook 'html-hook)
(add-hook 'c-mode-common-hook 'cc-hook)
(add-hook 'go-mode-hook 'go-hook)
(add-hook 'rust-mode-hook 'rust-hook)

(autoload 'js3-mode "js3" nil t)
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jst$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.podspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js3-mode))
(add-to-list 'auto-mode-alist '("\\.tac$" . python-mode))
(add-to-list 'auto-mode-alist '("zshrc$" . sh-mode))

(custom-set-variables
 '(sp-base-key-bindings 'paredit)
 '(sp-autoskip-closing-pair 'always)
 '(uniquify-buffer-name-style 'forward)
 '(haskell-process-type 'cabal-repl)
 '(js-indent-level 2)
 '(js3-indent-on-enter-key t)
 '(js3-enter-indents-newline t)
 '(safe-local-variable-values '((erlang-indent-level . 4))))

(provide 'willy)
;;; willy.el ends here
