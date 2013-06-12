;; system
(add-to-list 'load-path "~/.emacs.d/vendor")
(defalias 'vi 'evil-mode)
(setq system-uses-terminfo nil)
(fset 'insert-pound "#")

(global-set-key (kbd "M-3") 'insert-pound)

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

;; defuns
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

(defun to-markdown ()
  (interactive)
  (shell-command (concat "markdown " buffer-file-name " | browser")))

(defun wly-switch-to-prev-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

(defun file-string (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun wly-nrepl ()
  (interactive)
  (let ((port (file-string (expand-file-name (concat (ffip-project-root) "target/repl-port")))))
    (nrepl "localhost" port)))

(defun wly-switch-to-or-open-shell ()
  (interactive)
  (let ((buf (find-buffer-visiting "*terminal*")))
    (if buf
        (switch-to-buffer-other-window buf)
      (term "zsh"))))

;; packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(defun ensure-package (package-name)
  (unless (package-installed-p package-name)
    (package-refresh-contents)
    (package-install package-name)))
(ensure-package 'evil)
(ensure-package 'undo-tree)
(ensure-package 'key-chord)
(ensure-package 'ido-ubiquitous)
(ensure-package 'find-file-in-project)
(ensure-package 'paredit)
(ensure-package 'lein)
(ensure-package 'nrepl)
(ensure-package 'clojure-mode)
(ensure-package 'clojure-test-mode)
(ensure-package 'nrepl-ritz)
(ensure-package 'coffee-mode)
(ensure-package 'ack)
(ensure-package 'color-theme-solarized)
(ensure-package 'scss-mode)
(ensure-package 'markdown-mode)
(ensure-package 'linum)
(ensure-package 'flycheck)
(ensure-package 'autopair)

;; ido
(setq ido-enable-flex-matching t)
(ido-mode 1)

;; autocomplete
(require 'auto-complete)
(global-auto-complete-mode t)
;; (add-to-list 'ac-modes 'sql-mode)
(add-to-list 'ac-modes 'coffee-mode)
(add-to-list 'ac-modes 'scss-mode)

;; scss
(defun setup-scss ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (setq scss-compile-at-save nil
        css-indent-offset 2
        tab-width 2)
  (autopair-mode t))
(add-hook 'scss-mode-hook 'setup-scss)

(defun setup-shell ()
  (setq tab-width 2
        sh-basic-offset 2
        sh-indentation 2)
  (electric-pair-mode t))
(add-hook 'sh-mode-hook 'setup-shell)

(defun setup-html ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (electric-pair-mode t))
(add-hook 'html-mode-hook 'setup-html)

;; coffee
(require 'find-file-in-project)
(add-to-list 'ffip-patterns "*.coffee")
(add-to-list 'ffip-patterns "*.scss")
(add-to-list 'ffip-patterns "*.jst")
(add-to-list 'ffip-patterns "*.json")
(add-to-list 'auto-mode-alist '("\\.jst\\'" . html-mode))

(defun build-find-excludes (patterns)
  (mapconcat '(lambda (pat)
                (format "-not -regex \"%s\"" pat))
             patterns
             " "))

(setq ffip-find-options (build-find-excludes '(".*.tmp.*" ".*autodoc.*" ".*node_modules.*"))
      ffip-full-paths t
      ffip-limit 1024)
(defun setup-coffee-mode ()
  (setq tab-width 2)
  (electric-pair-mode t))
(add-hook 'coffee-mode-hook 'setup-coffee-mode)

;; ruby
(add-to-list 'auto-mode-alist '("\\.podspec\\'" . ruby-mode))

;; zencoding
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

;; markdown
(autoload 'markdown-mode "markdown-mode"
          "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; aspell
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")

;; ack
(setq ack-default-directory-function '(lambda (&rest args)
                                        (ffip-project-root)))

;; willy
(load-theme 'solarized-dark t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq-default line-number-mode t
              column-number-mode t)
(linum-mode -1)
(add-hook 'after-init-hook #'global-flycheck-mode)

(global-whitespace-mode)
(setq whitespace-action '(auto-cleanup))
(setq whitespace-style '(trailing space-before-tab space-after-tab indentation empty lines-tail))
(setq-default tab-width 2)

(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq backup-by-copying t ;; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/backups/emacs.backups")) ;; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t) ;; use versioned backups

;; evil
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode t)
(key-chord-mode t)
(evil-ex-define-cmd "W" 'evil-write)
(key-chord-define evil-insert-state-map (kbd "jk") 'evil-normal-state)
(define-key evil-normal-state-map (kbd "-") 'evil-window-next)
(define-key evil-normal-state-map (kbd "_") 'evil-window-prev)
(define-key evil-normal-state-map "\\`" 'wly-switch-to-prev-buffer)
(define-key evil-normal-state-map "\\b" 'ido-switch-buffer)
(define-key evil-normal-state-map "\\p" 'ffip)
(define-key evil-normal-state-map "\\s" 'wly-switch-to-or-open-shell)
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

;; lisps
(add-to-list 'load-path "~/.emacs.d/evil-paredit")
(require 'evil-paredit)
(defun setup-clojure-mode ()
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
    (fnk 1)))

(defun setup-emacs-lisp-mode ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (paredit-mode)
  (evil-paredit-mode)
  (rainbow-delimiters-mode))
(add-hook 'clojure-mode-hook 'setup-clojure-mode)
(add-hook 'emacs-lisp-mode-hook 'setup-emacs-lisp-mode)

(defun nrepl-interaction-mode-setup ()
  (require 'nrepl-ritz))
(add-hook 'nrepl-interaction-mode-hook 'nrepl-interaction-mode-setup)

;; nrepl
(require 'nrepl)
(define-key nrepl-mode-map (kbd "M-s") 'paredit-splice-sexp)
(define-key evil-normal-state-map (kbd "M-.") 'nrepl-jump)
;; (define-prefix-command 'nrepl-eval-map)
;; (key-chord-define evil-normal-state-map "cp" 'nrepl-eval-map)
;; (define-key 'nrepl-eval-map "e" 'nrepl-eval-expression-at-point)
;; (define-key 'nrepl-eval-map "p" 'nrepl-eval-buffer)
