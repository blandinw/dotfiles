;; -----------------------------------------------------------------------------
;; Packages

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))
(wy/ensure-packages '(evil
                      paredit
                      projectile
                      rainbow-delimiters
                      smartparens
                      yasnippet))

;; -----------------------------------------------------------------------------
;; Evil

(with-eval-after-load 'evil
  (define-key evil-visual-state-map "gc" 'comment-or-uncomment-region)
  (define-key evil-normal-state-map "gcc" '(lambda ()
                                             (interactive)
                                             (comment-or-uncomment-region (line-beginning-position)
                                                                          (line-end-position))))
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cb" 'org-iswitchb))

(setq evil-want-C-u-scroll t)
(require 'evil)

;; -----------------------------------------------------------------------------
;; Elisp

(defun wy/elisp-hook ()
  (rainbow-delimiters-mode 1))
(add-hook 'emacs-lisp-mode-hook 'wy/elisp-hook)

;; -----------------------------------------------------------------------------
;; C and C++

(defun wy/c-mode-common-hook ()
  )
(add-hook 'c-mode-common-hook 'wy/c-mode-common-hook)

;; -----------------------------------------------------------------------------
;; Shell

(defun wy/shell-hook ()
  )
(add-hook 'sh-mode-hook 'wy/shell-hook)
(add-to-list 'auto-mode-alist '("zshrc$" . sh-mode))

;; -----------------------------------------------------------------------------
;; Python

(add-to-list 'auto-mode-alist '("\\.tac$" . python-mode))
(add-to-list 'auto-mode-alist '("^BUCK$" . python-mode))

;; -----------------------------------------------------------------------------
;; Markdown

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; -----------------------------------------------------------------------------
;; Smartparens

(defun wy/create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(with-eval-after-load 'smartparens
  (sp-with-modes sp--lisp-modes
    (sp-local-pair "(" nil :bind "M-("))
  (sp-local-pair 'rust-mode "'" nil :actions nil)
  (sp-local-pair 'web-mode "{" nil
                 :post-handlers '((wy/create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'css-mode "{" nil
                 :post-handlers '((wy/create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'scss-mode "{" nil
                 :post-handlers '((wy/create-newline-and-enter-sexp "RET"))))

(require 'smartparens)

;; -----------------------------------------------------------------------------
;; General

(setq
 kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions)
 system-uses-terminfo nil
 whitespace-action '(auto-cleanup)
 whitespace-style '(trailing space-before-tab space-after-tab indentation empty lines-tail)
 )

(put 'dired-find-alternate-file 'disabled nil)

(linum-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(yas-global-mode 1)
(smartparens-global-mode 1)
(global-whitespace-mode 1)
(projectile-global-mode 1)

(add-hook 'after-save-hook 'wy/byte-compile-current-buffer)

(wy/config)
