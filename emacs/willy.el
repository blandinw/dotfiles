;; php-mode :<
(add-to-list 'load-path "~/.emacs.d/vendor")
(require 'php-mode)

;; coffee-mode :)
(add-to-list 'load-path "~/.emacs.d/vendor/coffee-mode")
(require 'coffee-mode)
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

;; slime-mode
(setq inferior-lisp-program "/usr/local/bin/sbcl") ; your Lisp system
(add-to-list 'load-path "~/.emacs.d/vendor/slime/")  ; your SLIME directory
(require 'slime)
(slime-setup '(slime-fancy))

;; vim-mode +_+
(add-to-list 'load-path "~/.emacs.d/vendor/vimpulse")
(require 'vimpulse)
(setq-default viper-auto-indent t)

;; textmate mode
(add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
(require 'textmate)
(textmate-mode)

;; aspell
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")

;; #
(fset 'insert-pound
   "#")
(global-set-key (kbd "M-3") 'insert-pound)

;; willy
(setq line-number-mode t)
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(global-whitespace-mode)
(setq whitespace-style '(face trailing tabs lines-tail tab-mark))

(load "blndw/theme.el")
