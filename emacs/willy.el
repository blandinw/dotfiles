;; php-mode :<
(add-to-list 'load-path "~/.emacs.d/vendor")
(require 'php-mode)

;; slime-mode
(setq inferior-lisp-program "/usr/local/bin/sbcl") ; your Lisp system
(add-to-list 'load-path "~/.emacs.d/vendor/slime/")  ; your SLIME directory
(require 'slime)
(slime-setup '(slime-fancy))

;; git
(add-to-list 'load-path "~/.emacs.d/vendor/magit")
(require 'magit)

;; textmate mode
(add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
(require 'textmate)
(textmate-mode)

;; zencoding
(add-to-list 'load-path "~/.emacs.d/vendor/zencoding")
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

;; aspell
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")

;; #
(fset 'insert-pound "#")
(global-set-key (kbd "M-3") 'insert-pound)

;; willy
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
(setq-default line-number-mode t)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(global-whitespace-mode)
(setq whitespace-style '(face trailing tabs lines-tail tab-mark))
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/backups/emacs.backups"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(add-to-list 'load-path "~/.emacs.d/blndw")
(load "coffeerc.el")
(load "vimrc.el")
(load "theme.el")

(server-start)

