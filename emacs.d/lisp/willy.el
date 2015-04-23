;; -----------------------------------------------------------------------------
;; Base

(defalias 'vi 'evil-mode)

(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/evil-paredit")

;; -----------------------------------------------------------------------------
;; Custom functions

(defun add-to-list* (the-list elems)
  (if elems
      (progn
        (add-to-list the-list (car elems))
        (add-to-list* the-list (cdr elems)))
    (eval the-list)))

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

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

;; -----------------------------------------------------------------------------
;; Packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(ensure-packages '(ac-cider
                   ac-emmet
                   ac-octave
                   ack
                   auto-complete
                   cider
                   clojure-mode
                   clojurescript-mode
                   cmake-mode
                   coffee-mode
                   color-theme
                   color-theme-solarized
                   dash
                   dockerfile-mode
                   elixir-mode
                   emmet-mode
                   epl
                   erlang
                   evil
                   evil-paredit
                   f
                   flycheck
                   flymake-easy
                   flymake-haskell-multi
                   fringe-helper
                   fuel
                   ghc
                   git-commit-mode
                   git-rebase-mode
                   go-autocomplete
                   go-mode
                   goto-chg
                   goto-last-change
                   haskell-mode
                   ido-ubiquitous
                   inf-ruby
                   javap-mode
                   js3-mode
                   json-mode
                   json-reformat
                   json-snatcher
                   key-chord
                   let-alist
                   linum
                   magit
                   markdown-mode
                   nginx-mode
                   notmuch
                   paredit
                   php-mode
                   pkg-info
                   popup
                   projectile
                   queue
                   rainbow-delimiters
                   ruby-end
                   rust-mode
                   s
                   scss-mode
                   smartparens
                   toml-mode
                   tree-mode
                   undo-tree
                   web-mode
                   yaml-mode
                   yasnippet))

(require 'auto-complete)
(require 'auto-complete-config)
(require 'cider)
(require 'css-mode)
(require 'evil)
(require 'evil-paredit)
(require 'projectile)
(require 'scss-mode)
(require 'smartparens-config)
(require 'uniquify)
(require 'yasnippet)

;; -----------------------------------------------------------------------------
;; Hooks

(defun cc-hook ()
  (smartparens-mode 1)
  (local-set-key (kbd "C-c C-k") 'compile)
  (local-set-key (kbd "RET") 'newline-and-indent))

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
    ;; wit
    (delta 'defun)
    (defcontroller 'defun)
    (nlp 'defun)
    (defsm 'defun)
    (doarr 'defun)
    (if-not-let 'defun)
    (try-nil 'defun)
    (stubbing-goog 'defun)
    (stubbing-stack 'defun)
    (dom/a 'defun)
    (div 'defun)
    (img 'defun)
    (input 'defun)
    (label 'defun)
    (li 'defun)
    (span 'defun)
    (textarea 'defun)
    (ul 'defun)
    (as-channel 'defun)
    (query 'defun)
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

(defun coffee-hook ()
  (smartparens-mode 1)
  (setq tab-width 2))

(defun css-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (setq css-indent-offset 2
        tab-width 2)
  (smartparens-mode 1)
  (emmet-mode t)
  (flycheck-mode -1)
  (define-key evil-insert-state-map (kbd "C-j") 'emmet-expand-yas)
  (local-set-key (kbd "C-j") 'emmet-expand-yas))

(defun elisp-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "C-c C-k") 'eval-buffer)

  (setq ac-sources (append '(
                             ac-source-features
                             ac-source-functions
                             ac-source-symbols
                             ac-source-variables
                             ac-source-yasnippet
                             )
                           ac-sources))

  (rainbow-delimiters-mode 1)

  (paredit-mode 1)
  (evil-paredit-mode 1)

  ;; fancy
  (font-lock-add-keywords
   'emacs-lisp-mode `(("(\\(lambda\\)[\[[:space:]]"
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "λ")
                                 nil))))))

(defun elixir-hook ()
  (smartparens-mode 1)
  (flycheck-mode -1)

  ;; end
  (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
       "\\(?:^\\|\\s-+\\)\\(?:do\\)")
  (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
  (ruby-end-mode t))

(defun go-hook ()
  (smartparens-mode 1)
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun haskell-hook ()
  (smartparens-mode 1)
  (turn-on-haskell-indentation)
  (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (ghc-init)
  (message "haskell mode loaded."))

(defun html-hook ()
  (smartparens-mode 1)
  (emmet-mode t)
  (flycheck-mode -1)
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun js-hook ()
  (smartparens-mode 1)
  (define-key evil-normal-state-map (kbd "M-.") 'find-tag)
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun php-hook ()
  (define-key evil-normal-state-map (kbd "M-.") 'find-tag))

(defun ruby-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (smartparens-mode 1))

(defun rust-hook ()
  (smartparens-mode 1)
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun scss-hook ()
  (css-hook)
  (setq scss-compile-at-save nil))

(defun sgml-hook ()
  (smartparens-mode 1)
  (emmet-mode t))

(defvar sh-basic-offset)
(defvar sh-indentation)
(defun shell-hook ()
  (smartparens-mode 1))

(defun web-hook ()
  (let ((cur (web-mode-language-at-pos)))
    (when (or (string= cur "javascript") (string= cur "jsx"))
      (js-hook))
    (when (string= cur "html")
      (html-hook))))

(add-hook 'after-save-hook 'byte-compile-current-buffer)
(add-hook 'cider-mode-hook 'cider-hook)
(add-hook 'cider-popup-buffer-mode-hook 'cider-popup-buffer-hook)
(add-hook 'cider-repl-mode-hook 'cider-repl-hook)
(add-hook 'clojure-mode-hook 'clojure-hook)
(add-hook 'clojurescript-mode-hook 'clojurescript-hook)
(add-hook 'c-mode-common-hook 'cc-hook)
(add-hook 'coffee-mode-hook 'coffee-hook)
(add-hook 'css-mode-hook 'css-hook)
(add-hook 'elixir-mode-hook 'elixir-hook)
(add-hook 'emacs-lisp-mode-hook 'elisp-hook)
(add-hook 'go-mode-hook 'go-hook)
(add-hook 'haskell-mode-hook 'haskell-hook)
(add-hook 'html-mode-hook 'html-hook)
(add-hook 'javascript-mode-hook 'js-hook)
(add-hook 'js2-mode-hook 'js-hook)
(add-hook 'js3-mode-hook 'js-hook)
(add-hook 'php-mode-hook 'php-hook)
(add-hook 'ruby-mode-hook 'ruby-hook)
(add-hook 'rust-mode-hook 'rust-hook)
(add-hook 'scss-mode-hook 'scss-hook)
(add-hook 'sgml-mode-hook 'sgml-hook) ;; Auto-starts on any markup modes
(add-hook 'sh-mode-hook 'shell-hook)
(add-hook 'web-mode-hook 'web-hook)

(autoload 'js3-mode "js3" nil t)
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.[agj]sp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jst$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.podspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.react\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tac$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php" . web-mode))
(add-to-list 'auto-mode-alist '("zshrc$" . sh-mode))

(eval-after-load 'cider
  '(progn (setq cider-repl-pop-to-buffer-on-connect nil
                cider-popup-stacktraces t
                cider-repl-popup-stacktraces t
                cider-auto-select-error-buffer t
                cider-repl-wrap-history t
                cider-repl-history-size 1000
                cider-repl-history-file "/tmp/cider-repl-history")))


;; -----------------------------------------------------------------------------
;; Keys: xterm compatibility

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

;; -----------------------------------------------------------------------------
;; Keys: Evil bindings

(evil-mode t)
(key-chord-mode t)
(evil-ex-define-cmd "W" 'evil-write)
(key-chord-define evil-insert-state-map (kbd "jk") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "<enter>") 'evil-ret-and-indent)
(define-key evil-normal-state-map (kbd "-") 'evil-window-next)
(define-key evil-normal-state-map (kbd "_") 'evil-window-prev)
(define-key evil-normal-state-map "\\`" 'evil-buffer)
(define-key evil-normal-state-map "\\b" 'projectile-switch-to-buffer)
(define-key evil-normal-state-map "\\p" 'projectile-find-file)
(define-key evil-normal-state-map "\\g" 'magit-status)
(define-key evil-normal-state-map "\\s" 'wly/switch-to-or-open-shell)
(define-key evil-normal-state-map "\\ve" (lambda ()
                                           (interactive)
                                           (find-file "~/.emacs.d/lisp/willy.el")))
(define-key evil-normal-state-map "\\z" 'evil-emacs-state)
(define-key evil-normal-state-map "\\m" 'to-markdown)
(define-key evil-normal-state-map (kbd "C-z") 'suspend-frame)
(define-key evil-visual-state-map "gc" 'comment-or-uncomment-region)
(define-key evil-normal-state-map "gcc" '(lambda ()
                                           (interactive)
                                           (comment-or-uncomment-region (line-beginning-position)
                                                                        (line-end-position))))

(define-key evil-normal-state-map "\\t" '(lambda ()
                                           (interactive)
                                           (wly/load-theme (if (eq 'solarized-dark loaded-theme)
                                                               'solarized-light
                                                             'solarized-dark))))

(global-set-key (kbd "RET") 'newline-and-indent)

;; -----------------------------------------------------------------------------
;; Customizations

(require 'web-mode)

(setq
 ack-default-directory-function '(lambda (&rest args) (projectile-project-root))
 backup-by-copying t ;; don't clobber symlinks
 backup-directory-alist '(("." . "~/backups/emacs.backups")) ;; don't litter my fs tree
 column-number-mode t
 custom-file "~/.emacs.d/custom.el"
 delete-old-versions t
 ido-enable-flex-matching t
 indent-tabs-mode nil
 inhibit-startup-echo-area-message t
 inhibit-startup-message t
 ispell-program-name "aspell"
 ispell-list-command "list"
 kept-new-versions 6
 kept-old-versions 2
 kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions)
 line-number-mode t
 magit-last-seen-setup-instructions "1.4.0"
 system-uses-terminfo nil
 tab-width 2
 version-control t ;; use versioned backups
 web-mode-content-types-alist '(("jsx" . "\\.react\\.js$"))
 whitespace-action '(auto-cleanup)
 whitespace-style '(trailing space-before-tab space-after-tab indentation empty lines-tail)
 )

(put 'dired-find-alternate-file 'disabled nil)

(global-auto-complete-mode t)
(global-whitespace-mode t)
(ido-mode t)
(linum-mode -1)
(menu-bar-mode -1)
(projectile-global-mode 1)
(tool-bar-mode -1)
(yas-global-mode 1)

(wly/load-theme 'solarized-light)

;; -----------------------------------------------------------------------------
;; Smartparens

(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "M-("))

(sp-local-pair 'rust-mode "'" nil :actions nil)

;; -----------------------------------------------------------------------------
;; Autocomplete

(add-to-list* 'ac-modes '(
                          cider-mode
                          cider-repl-mode
                          coffee-mode
                          elixir-mode
                          haskell-mode
                          rust-mode
                          scss-mode
                          ))

;; -----------------------------------------------------------------------------
;; Allow local customizations

(load custom-file)

(let ((local (locate-library "local")))
  (when local (load local)))

(provide 'willy)
;;; willy.el ends here
