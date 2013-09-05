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
              c-basic-offset 4
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

(defun wly/nrepl ()
  (interactive)
  (let* ((file-path (expand-file-name "~/.lein/repl-port"))
         (port (file-string file-path)))
    (nrepl "localhost" port)))

(defun wly/switch-to-or-open-shell ()
  (interactive)
  (let ((buf (find-buffer-visiting "*terminal*")))
    (if buf
        (switch-to-buffer-other-window buf)
      (term "zsh"))))

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

(defmacro ret-indent ()
  '(local-set-key (kbd "RET") 'newline-and-indent))

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(ensure-packages '(ack
                   autopair
                   auto-complete
                   clojure-mode
                   clojure-test-mode
                   coffee-mode
                   color-theme-solarized
                   elixir-mode
                   erlang
                   evil
                   find-file-in-project
                   flycheck
                   haskell-mode
                   ido-ubiquitous
                   js3
                   key-chord
                   lein
                   linum
                   markdown-mode
                   nginx-mode
                   nrepl
                   nrepl-ritz
                   paredit
                   rainbow-delimiters
                   ruby-end
                   scss-mode
                   undo-tree
                   zencoding-mode))

(require 'evil)
(require 'evil-paredit)
(require 'auto-complete)
(require 'nrepl)
(require 'find-file-in-project)

(define-key input-decode-map "\e3" "#")
(define-key input-decode-map "\e[18~" (kbd "<C-S-backspace>"))
(define-key input-decode-map "\e[9;98" (kbd "C-S-("))
(define-key input-decode-map "\eOA" (kbd "<up>"))
(define-key input-decode-map "\eOB" (kbd "<down>"))
(define-key input-decode-map "\eOC" (kbd "<right>"))
(define-key input-decode-map "\eOD" (kbd "<left>"))
(define-key input-decode-map "\e[A" (kbd "<C-up>"))
(define-key input-decode-map "\e[B" (kbd "<C-down>"))
(define-key input-decode-map "\e[C" (kbd "<C-right>"))
(define-key input-decode-map "\e[D" (kbd "<C-left>"))

;; javascript
(defun js-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent))

;; scss
(defun scss-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (setq scss-compile-at-save nil
        css-indent-offset 2
        tab-width 2)
  (autopair-mode t)
  (electric-pair-mode -1))

(defun shell-hook ()
  (setq tab-width 2
        sh-basic-offset 2
        sh-indentation 2)
  (electric-pair-mode t))

(defun html-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (zencoding-mode t)
  (flycheck-mode -1)
  (electric-pair-mode t))

(defun coffee-hook ()
  (setq tab-width 2)
  (electric-pair-mode t))

(defun ruby-hook ()
  (autopair-mode t)
  (ruby-end-mode t))

(defun sgml-hook ()
  (zencoding-mode t))

(defun clojure-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)

  (paredit-mode)
  (evil-paredit-mode)
  (rainbow-delimiters-mode)
  (define-clojure-indent
    (defprotocol 'defun)
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)
    (against-background 1)
    (background 0)
    (fact 1)
    (facts 1)
    (fnk 1)
    (delta 'defun)
    (defcontroller 'defun)
    (doarr 'doseq))

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

(defun elisp-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (paredit-mode)
  (evil-paredit-mode)
  (rainbow-delimiters-mode))

(defun nrepl-hook ()
  (require 'nrepl-ritz))

(defun elixir-hook ()
  (autopair-mode t)
  (flycheck-mode -1)

  ;; end
  (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
       "\\(?:^\\|\\s-+\\)\\(?:do\\)")
  (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
  (ruby-end-mode t))

(defun haskell-hook ()
  (turn-on-haskell-indentation)
  (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (message "haskell mode loaded."))

(add-to-list* 'ffip-patterns '("*.coffee"
                               "*.scss"
                               "*.jst"
                               "*.jsx"
                               "*.json"
                               "*.java"
                               "*.yml"
                               "*.j2"
                               "*.cfg"
                               "*.markdown"
                               "*.md"
                               "*.ex"
                               "*.exs"))

(defun build-find-excludes (patterns)
  (mapconcat '(lambda (pat)
                (format "-not -regex \"%s\"" pat))
             patterns
             " "))

(setq ffip-find-options (build-find-excludes '(".*.tmp.*"
                                               ".*autodoc.*"
                                               ".*node_modules.*"
                                               ".*app/components.*"
                                               ".*/cabal-dev/.*"
                                               ".*/dist/.*"))
      ffip-full-paths t
      ffip-limit 1024)

;; evil
(evil-mode t)
(key-chord-mode t)
(evil-ex-define-cmd "W" 'evil-write)
(key-chord-define evil-insert-state-map (kbd "jk") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "<enter>") 'evil-ret-and-indent)
(define-key evil-normal-state-map (kbd "-") 'evil-window-next)
(define-key evil-normal-state-map (kbd "_") 'evil-window-prev)
(define-key evil-normal-state-map "\\`" 'wly/switch-to-prev-buffer)
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
(define-key evil-normal-state-map "\\n" '(lambda ()
                                           (interactive)
                                           (wly/nrepl)
                                           (kill-buffer "*nrepl*")))
(define-key evil-normal-state-map (kbd "M-.") 'nrepl-jump)

(define-key evil-visual-state-map (kbd "C-<up>") 'mc/mark-previous-like-this)
(define-key evil-visual-state-map (kbd "C-<down>") 'mc/mark-next-like-this)
(define-key nrepl-mode-map (kbd "M-s") 'paredit-splice-sexp)

;; post
(load-theme 'solarized-dark t)
(setq whitespace-action '(auto-cleanup)
      whitespace-style '(trailing space-before-tab space-after-tab indentation empty lines-tail))

(setq ido-enable-flex-matching t)
(ido-mode t)

(global-auto-complete-mode t)
(add-to-list* 'ac-modes '(coffee-mode scss-mode elixir-mode))

(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ack-default-directory-function '(lambda (&rest args)
                                        (ffip-project-root)))

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'emacs-lisp-mode-hook 'elisp-hook)
(add-hook 'clojure-mode-hook 'clojure-hook)
(add-hook 'elixir-mode-hook 'elixir-hook)
(add-hook 'haskell-mode-hook 'haskell-hook)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-hook)
(add-hook 'sgml-mode-hook 'sgml-hook) ;; Auto-starts on any markup modes
(add-hook 'ruby-mode-hook 'ruby-hook)
(add-hook 'coffee-mode-hook 'coffee-hook)
(add-hook 'javascript-mode-hook 'js-hook)
(add-hook 'scss-mode-hook 'scss-hook)
(add-hook 'html-mode-hook 'html-hook)
(add-hook 'sh-mode-hook 'shell-hook)
(add-hook 'web-mode-hook 'html-hook)

(autoload 'js3-mode "js3" nil t)
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jst$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.podspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js3-mode))

(custom-set-variables
 '(haskell-process-type 'cabal-dev)
 '(js3-indent-on-enter-key t)
 '(js3-enter-indents-newline t)
 '(safe-local-variable-values '((erlang-indent-level . 4))))
