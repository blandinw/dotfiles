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

(defun wly/remove-autosave-file ()
  (interactive)
  (let ((f (make-auto-save-file-name)))
    (message "deleting autosave file \"%s\"" f)
    (delete-file f)))

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
  (define-key evil-normal-state-map "\\t" '(lambda () (interactive)
                                             (let ((buf "todo.org"))
                                               (if (buffer-live-p (get-buffer buf))
                                                   (switch-to-buffer buf)
                                                 (message "could not find buffer \"%s\"" buf)))))
  (define-key evil-normal-state-map "\\ve" (lambda ()
                                             (interactive)
                                             (find-library "willy")))
  (define-key evil-normal-state-map "\\z" 'evil-emacs-state)
  (define-key evil-normal-state-map "\\m" 'wly/to-markdown)
  (define-key evil-normal-state-map (kbd "C-z") 'suspend-frame)
  (define-key evil-normal-state-map (kbd "<escape>") '(lambda ()
                                                        (interactive)
                                                        (evil-ex-nohighlight)
                                                        (keyboard-quit))))

;; -----------------------------------------------------------------------------
;; Org-mode

(with-eval-after-load 'org-mode
  (add-hook 'org-mode-hook (lambda ()
                             (smartparens-mode 0))))

;; -----------------------------------------------------------------------------
;; C/C++

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Smartparens is broken in `cc-mode' as of Emacs 27. See
            ;; <https://github.com/Fuco1/smartparens/issues/963>.
            (when (version<= "27" emacs-version)
              (dolist (fun '(c-electric-paren c-electric-brace))
                (add-to-list 'sp--special-self-insert-commands fun)))

            (unless (file-exists-p "Makefile")
              (set (make-local-variable 'compile-command)
                   (cond ((file-exists-p "BUCK") "buck build : :everything#compilation-database"))))))

(defun wly/config ()
  (blink-cursor-mode 0)
  (when (not (display-graphic-p))
    (xterm-mouse-mode 0)))

;; -----------------------------------------------------------------------------
;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (paredit-mode 1)))

;; -----------------------------------------------------------------------------
;; Load vanilla Emacs config if no Spacemacs

(when (not (boundp 'spacemacs-version))
  (progn (load "no-spacemacs")
         (wly/config)))

;; -----------------------------------------------------------------------------
;; Allow local customizations

(let ((emacsd-directory (file-name-directory (directory-file-name (file-name-directory load-file-name))))
      (local-file (locate-library "local")))
  (when local-file (load local-file))

  (setq custom-file (concat emacsd-directory "custom.el"))
  (if (not (boundp 'local-custom))
      ;; no local custom, just load regular file
      (load custom-file)
    (let ((generated-custom-file (concat emacsd-directory
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
