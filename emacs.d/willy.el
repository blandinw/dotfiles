;; -----------------------------------------------------------------------------
;; Base

(defalias 'vi 'evil-mode)

;; -----------------------------------------------------------------------------
;; Helpers

(defun wly/add-to-list* (the-list elems)
  (if elems
      (progn
        (add-to-list the-list (car elems))
        (wly/add-to-list* the-list (cdr elems)))
    (eval the-list)))

(defun wly/byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(defun wly/to-markdown ()
  (interactive)
  (shell-command (concat "marked --gfm " buffer-file-name " | browser")))

(defun wly/switch-to-prev-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

(defun wly/file-string (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun wly/switch-to-or-open-shell ()
  (interactive)
  (let ((buf (find-buffer-visiting "*terminal*")))
    (if buf
        (switch-to-buffer-other-window buf)
      (term "zsh"))))

(defun wly/ensure-package (package-name)
  (unless (package-installed-p package-name)
    (package-refresh-contents)
    (package-install package-name)))

(defun wly/ensure-packages (packages)
  (if packages
      (progn
        (wly/ensure-package (car packages))
        (wly/ensure-packages (cdr packages)))
    nil))

;; -----------------------------------------------------------------------------
;; Keys: xterm compatibility, has to be in hooked to tty-setup for emacsclient

(add-hook
  'tty-setup-hook
  (lambda ()
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
    (define-key input-decode-map "\e[1;7B" (kbd "<M-down>"))))

;; -----------------------------------------------------------------------------
;; Keys: Evil bindings

(with-eval-after-load 'evil
  (evil-mode t)
  (evil-ex-define-cmd "W" 'evil-write)
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
                                             (find-library "willy")))
  (define-key evil-normal-state-map "\\z" 'evil-emacs-state)
  (define-key evil-normal-state-map "\\m" 'wly/to-markdown)
  (define-key evil-normal-state-map (kbd "C-z") 'suspend-frame))

(defun wly/config ()
  (when (not (display-graphic-p))
    (xterm-mouse-mode 0)))

;; -----------------------------------------------------------------------------
;; Spacemacs or fallback

(if (not (boundp 'spacemacs-version))
    (progn (load "no-spacemacs")
           (wly/config))
  ;; Spacemacs-specific
  (progn
    (add-hook
     'spacemacs-post-user-config-hook
     (lambda ()
       (wly/config)
       (setq-default evil-escape-key-sequence "jk"
                     persp-auto-save-opt 0)
       (setq powerline-default-separator 'bar)
       (setq haskell-stylish-on-save t) ;; override haskell-mode hardcoded
       (spaceline-compile)
       (helm-projectile-on)
       ;(define-key evil-insert-state-map (kbd "<tab>") 'yas-expand)
       ))))

;; -----------------------------------------------------------------------------
;; Allow local customizations

(let ((this-directory (file-name-directory load-file-name))
      (local-file (locate-library "local")))
  (when local-file (load local-file))

  (setq custom-file (concat this-directory "custom.el"))
  (if (not (boundp 'local-custom))
      ;; no local custom, just load regular file
      (load custom-file)
    (let ((generated-custom-file (concat this-directory
                                         "local-generated-custom.el")))
        ;; regenerate?
        (when (or (file-newer-than-file-p custom-file generated-custom-file)
                  (file-newer-than-file-p local-file generated-custom-file))
          (with-temp-file generated-custom-file
            (let* ((custom (car (read-from-string (wly/file-string custom-file))))
                   (merged (append custom local-custom)))
              (prin1 merged (current-buffer)))))
        (load generated-custom-file))))

(provide 'willy)
;;; willy.el ends here
