;; -----------------------------------------------------------------------------
;; Packages

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(wy/ensure-packages '(evil
                      evil-paredit
                      key-chord
                      paredit
                      projectile
                      rainbow-delimiters
                      smartparens
                      yasnippet))

(require 'evil-paredit)
(require 'projectile)
(require 'smartparens-config)
(require 'yasnippet)

;; -----------------------------------------------------------------------------
;; Elisp

(defun elisp-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "C-c C-k") 'eval-buffer)
  (rainbow-delimiters-mode 1)
  (evil-paredit-mode 1))
(add-hook 'emacs-lisp-mode-hook 'elisp-hook)

;; -----------------------------------------------------------------------------
;; C and C++

(defun cc-hook ()
  (smartparens-mode 1)
  (local-set-key (kbd "C-c C-k") 'compile)
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun c++-hook ()
  (smartparens-mode 1))
(add-hook 'c-mode-common-hook 'cc-hook)

;; -----------------------------------------------------------------------------
;; Web

(defun css-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (setq css-indent-offset 2
        tab-width 2)
  (smartparens-mode 1)
  (emmet-mode t)
  (flycheck-mode -1)
  (define-key evil-insert-state-map (kbd "C-j") 'emmet-expand-yas)
  (local-set-key (kbd "C-j") 'emmet-expand-yas))

(defun html-hook ()
  (smartparens-mode 1)
  (emmet-mode t)
  (flycheck-mode -1)
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun js-hook ()
  (smartparens-mode 1)
  (define-key evil-normal-state-map (kbd "M-.") 'find-tag)
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun scss-hook ()
  (css-hook)
  (setq scss-compile-at-save nil))

(defun web-hook ()
  (let ((cur (web-mode-language-at-pos)))
    (when (or (string= cur "javascript") (string= cur "jsx"))
      (js-hook))
    (when (string= cur "html")
      (html-hook))))

(add-hook 'css-mode-hook 'css-hook)
(add-hook 'html-mode-hook 'html-hook)
(add-hook 'javascript-mode-hook 'js-hook)
(add-hook 'js2-mode-hook 'js-hook)
(add-hook 'scss-mode-hook 'scss-hook)
(add-hook 'web-mode-hook 'web-hook)

;; (add-to-list 'auto-mode-alist '("\\.[agj]sp$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.json$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jst$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.react\\.js$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php" . web-mode))

;; -----------------------------------------------------------------------------
;; Haskell

(defun haskell-hook ()
  ;; do not include (flymake-mode)
  (smartparens-mode 1)
  (turn-on-haskell-decl-scan)
  (turn-on-haskell-doc-mode)
  (haskell-auto-insert-module-template)
  (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)
  (define-key haskell-mode-map (kbd "M-p") 'haskell-goto-prev-error)
  (define-key haskell-mode-map (kbd "M-n") 'haskell-goto-next-error)
  (define-key evil-normal-state-map (kbd "M-.") 'haskell-mode-jump-to-def)
  (interactive-haskell-mode)
  (speedbar-add-supported-extension ".hs")
  ;; (ghc-init)
  (set (make-local-variable 'company-backends)
       (append '((company-capf company-dabbrev-code))
               company-backends))
  (message "haskell mode loaded."))
(add-hook 'haskell-mode-hook 'haskell-hook)
;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)

;; -----------------------------------------------------------------------------
;; Clojure

(defun cider-hook ()
  (cider-turn-on-eldoc-mode))

(defun cider-repl-hook ()
  (rainbow-delimiters-mode 1)
  (paredit-mode 1)
  (evil-paredit-mode 1))

(defun cider-popup-buffer-hook ()
  (evil-local-set-key 'normal (kbd "q") 'cider-popup-buffer-quit-function))

(defun clojure-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (define-key evil-normal-state-map (kbd "M-.") 'cider-find-var)

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
(add-hook 'cider-mode-hook 'cider-hook)
(add-hook 'cider-popup-buffer-mode-hook 'cider-popup-buffer-hook)
(add-hook 'cider-repl-mode-hook 'cider-repl-hook)
(add-hook 'clojure-mode-hook 'clojure-hook)
(add-hook 'clojurescript-mode-hook 'clojurescript-hook)

(eval-after-load 'cider
 '(progn (setq cider-repl-pop-to-buffer-on-connect nil
             cider-popup-stacktraces t
             cider-repl-popup-stacktraces t
             cider-auto-select-error-buffer t
             cider-repl-wrap-history t
             cider-repl-history-size 1000
             cider-repl-history-file "/tmp/cider-repl-history")))

;; -----------------------------------------------------------------------------
;; Elixir

(defun elixir-hook ()
  (smartparens-mode 1)
  (flycheck-mode -1)

  ;; end
  (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
       "\\(?:^\\|\\s-+\\)\\(?:do\\)")
  (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
  (ruby-end-mode t))
(add-hook 'elixir-mode-hook 'elixir-hook)

;; -----------------------------------------------------------------------------
;; Go

(defun go-hook ()
  (smartparens-mode 1)
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'go-mode-hook 'go-hook)

;; -----------------------------------------------------------------------------
;; Lua

(defun lua-hook ()
  (smartparens-mode 1)
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'lua-mode-hook 'lua-hook)

;; -----------------------------------------------------------------------------
;; Ruby

(defun ruby-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (smartparens-mode 1))
(add-hook 'ruby-mode-hook 'ruby-hook)
(add-to-list 'auto-mode-alist '("\\.podspec$" . ruby-mode))

;; -----------------------------------------------------------------------------
;; Rust

(defun rust-hook ()
  (smartparens-mode 1)
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'rust-mode-hook 'rust-hook)

;; -----------------------------------------------------------------------------
;; SGML

(defun sgml-hook ()
  (smartparens-mode 1)
  (emmet-mode t))
(add-hook 'sgml-mode-hook 'sgml-hook) ;; auto-starts on any markup modes

;; -----------------------------------------------------------------------------
;; Shell

(defvar sh-basic-offset)
(defvar sh-indentation)
(defun shell-hook ()
  (smartparens-mode 1))
(add-hook 'sh-mode-hook 'shell-hook)
(add-to-list 'auto-mode-alist '("zshrc$" . sh-mode))


;; -----------------------------------------------------------------------------
;; Python

(add-to-list 'auto-mode-alist '("\\.tac$" . python-mode))

;; -----------------------------------------------------------------------------
;; Markdown

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; -----------------------------------------------------------------------------
;; Global hooks

;; (add-hook 'after-init-hook '(lambda () (global-company-mode)))
(add-hook 'after-save-hook 'wy/byte-compile-current-buffer)

;; -----------------------------------------------------------------------------
;; Keys: Evil bindings

(key-chord-mode t)
(key-chord-define evil-insert-state-map (kbd "jk") 'evil-normal-state)
(define-key evil-visual-state-map "gc" 'comment-or-uncomment-region)
(define-key evil-normal-state-map "gcc" '(lambda ()
                                           (interactive)
                                           (comment-or-uncomment-region (line-beginning-position)
                                                                        (line-end-position))))


(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(wy/add-to-list* 'evil-emacs-state-modes '(haskell-error-mode))

;; -----------------------------------------------------------------------------
;; Customizations

(setq
 ispell-list-command "list"
 kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions)
 system-uses-terminfo nil
 web-mode-content-types-alist '(("jsx" . "\\.react\\.js$"))
 whitespace-action '(auto-cleanup)
 whitespace-style '(trailing space-before-tab space-after-tab indentation empty lines-tail)
 )

(put 'dired-find-alternate-file 'disabled nil)

(global-whitespace-mode t)
(ido-mode t)
(linum-mode -1)
(menu-bar-mode -1)
(projectile-global-mode 1)
(tool-bar-mode -1)
(yas-global-mode 1)

;; -----------------------------------------------------------------------------
;; Smartparens

(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "M-("))

(sp-local-pair 'rust-mode "'" nil :actions nil)
(sp-local-pair 'web-mode "{" nil
               :post-handlers '((wy/create-newline-and-enter-sexp "RET")))
(sp-local-pair 'css-mode "{" nil
               :post-handlers '((wy/create-newline-and-enter-sexp "RET")))
(sp-local-pair 'scss-mode "{" nil
               :post-handlers '((wy/create-newline-and-enter-sexp "RET")))

(defun wy/create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))
