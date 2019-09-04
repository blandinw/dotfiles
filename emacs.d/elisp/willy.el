;; -----------------------------------------------------------------------------
;; Base

(defalias 'vi 'evil-mode)

;; -----------------------------------------------------------------------------
;; Helpers

(defun wy/add-to-list* (the-list elems)
  (if elems
      (progn
        (add-to-list the-list (car elems))
        (wy/add-to-list* the-list (cdr elems)))
    (eval the-list)))

(defun wy/byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(defun wy/to-markdown ()
  (interactive)
  (shell-command (concat "marked --gfm " buffer-file-name " | browser")))

(defun wy/switch-to-prev-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

(defun wy/file-string (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun wy/switch-to-or-open-shell ()
  (interactive)
  (let ((buf (find-buffer-visiting "*terminal*")))
    (if buf
        (switch-to-buffer-other-window buf)
      (term "zsh"))))

(defun wy//ensure-packages (packages)
  (if packages
      (let ((pkg (car packages)))
        (unless (package-installed-p pkg)
          (package-install pkg))
        (wy//ensure-packages (cdr packages)))
    nil))

(defun wy/ensure-packages (packages)
  (package-refresh-contents)
  (wy//ensure-packages packages))

(defun wy/delete-autosave-file ()
  (interactive)
  (let ((f (make-auto-save-file-name)))
    (message "deleting autosave file \"%s\"" f)
    (delete-file f)))

(defun wy/org-projectile-find-project-orgfile (project-path)
  (let ((gitrepo (s-trim (shell-command-to-string "git config --get remote.origin.url"))))
    (cond ((not (string-empty-p gitrepo)) (concat (file-name-base gitrepo) ".org"))
          (t (concat (file-name-base (directory-file-name project-path)) ".org")))))

(defvar wy/after-config-hook)
(defun wy/config ()
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
    (evil-mode 1)
    (evil-ex-define-cmd "W" 'evil-write)
    (define-key evil-insert-state-map (kbd "<enter>") 'evil-ret-and-indent)
    (define-key evil-normal-state-map (kbd "-") 'evil-window-next)
    (define-key evil-normal-state-map (kbd "_") 'evil-window-prev)
    (define-key evil-normal-state-map "\\`" 'evil-buffer)
    (define-key evil-normal-state-map "\\b" 'projectile-switch-to-buffer)
    (define-key evil-normal-state-map "\\p" 'projectile-find-file)
    (define-key evil-normal-state-map "\\g" 'magit-status)
    (define-key evil-normal-state-map "\\s" 'wy/switch-to-or-open-shell)
    (define-key evil-normal-state-map "\\t" '(lambda () (interactive)
                                               (let ((buf "todo.org"))
                                                 (if (buffer-live-p (get-buffer buf))
                                                     (switch-to-buffer buf)
                                                   (message "could not find buffer \"%s\"" buf)))))
    (define-key evil-normal-state-map "\\ve" (lambda ()
                                               (interactive)
                                               (find-library "willy")))
    (define-key evil-normal-state-map "\\z" 'evil-emacs-state)
    (define-key evil-normal-state-map "\\m" 'wy/to-markdown)
    (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
    ;; (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt) ;; clashes with Emacs prefixes
    (define-key evil-normal-state-map (kbd "C-z") 'suspend-frame)
    (define-key evil-normal-state-map (kbd "<escape>") '(lambda ()
                                                          (interactive)
                                                          (evil-ex-nohighlight)
                                                          (when (functionp 'pupo/close-window)
                                                            (pupo/close-window))
                                                          (keyboard-quit))))

  ;; -----------------------------------------------------------------------------
  ;; Projectile

  (with-eval-after-load 'projectile
    (wy/add-to-list* 'projectile-globally-ignored-directories
                     '(".vs" ".vscode" "buck-out" ".ccls-cache"))

    (when (eq system-type 'windows-nt)
      (setq projectile-indexing-method 'hybrid)
      (setq projectile-generic-command "wsl find . -type f -printf '%P\\0'")
      ;; NOTE(willy) monkey-patching vcs detection to avoid buggy git logic on Windows
      (setf (symbol-function 'projectile-project-vcs) '(lambda (&optional _) 'none))))

  ;; -----------------------------------------------------------------------------
  ;; Org-mode

  (with-eval-after-load 'org
    (setq org-capture-templates '(("j" "Journal entry" entry (file wy/org-journal)
                                   "* %T\n%?")
                                  ("t" "Task" entry (file+headline "" "Tasks")
                                   "* TODO %?\n  %u\n  %a"))
          org-refile-targets '((org-agenda-files :maxlevel . 3))
          org-projectile-per-project-filepath 'wy/org-projectile-find-project-orgfile
          org-blank-before-new-entry '((heading) (plain-list-item))
          org-catch-invisible-edits 'smart)
    (add-hook 'org-mode-hook (lambda ()
                               (smartparens-mode 0))))

  ;; -----------------------------------------------------------------------------
  ;; Emacs Lisp

  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (paredit-mode 1)))

  ;; -----------------------------------------------------------------------------
  ;; C/C++

  (add-hook 'c-mode-common-hook
            (lambda ()
              ;; Smartparens is broken in `cc-mode' as of Emacs 27. See
              ;; <https://github.com/Fuco1/smartparens/issues/963>.
              (when (version<= "27" emacs-version)
                (dolist (fun '(c-electric-paren c-electric-brace))
                  (add-to-list 'sp--special-self-insert-commands fun)))

              ;; guess compile command
              (let ((base-dir (if (and (boundp 'projectile-project-root) projectile-project-root)
                                  projectile-project-root
                                (file-name-directory buffer-file-name))))
                (set (make-local-variable 'compile-command)
                     (cond
                      ((file-exists-p "Makefile") "make -k")

                      ((and (eq system-type 'windows-nt)
                            (file-exists-p (format "%s/windows/build.bat" base-dir)))
                       (format "cmd.exe /c cd %s ^&^& windows\\build.bat" base-dir))

                      ((file-executable-p (format "%s/build.sh" base-dir))
                       (format "cd %s && ./build.sh" base-dir)))))))

  (setq wy/ccls-path-mapping '())

  (defun wy/window-path-to-unix-uri (path)
    ;; NOTE(willy) take into account the extra slash from windows-style URIs
    (let* ((win-uri (lsp--path-to-uri-1 path))
           (kv (cl-find-if (lambda (x)
                             (let ((k (car x))
                                   (v (cdr x)))
                               (string-prefix-p (concat "file:///" v) win-uri)))
                           wy/ccls-path-mapping)))
      (concat "file://" (car kv) (substring win-uri (length (concat "file:///" (cdr kv)))))))

  (defun wy/unix-uri-to-windows-path (uri)
    ;; NOTE(willy) root slash got removed from the path, as it was considered
    ;; part of the file:/// win-style file URI protocol. we add it back for consistency
    (let* ((win-path (lsp--uri-to-path-1 uri))
           (unix-path (concat "/" win-path))
           (kv (cl-find-if (lambda (x)
                             (let ((k (car x))
                                   (v (cdr x)))
                               ;; NOTE(willy) take into account the extra slash from windows-style URIs
                               (string-prefix-p k unix-path)))
                           wy/ccls-path-mapping)))
      (concat (cdr kv) (substring unix-path (length (car kv))))))

  (with-eval-after-load 'ccls
    (if-let ((ccls-client (gethash 'ccls lsp-clients)))
        ;; FIXME(willy) should work the same as aset, but crashes with "symbol function void"
        ;; try again in Emacs 27 stable
        ;; (setf (lsp--client-path->uri-fn ccls-client) 'wy/window-path-to-unix-uri
        ;;       (lsp--client-uri->path-fn ccls-client) 'wy/unix-uri-to-windows-path)
        (progn
          (aset ccls-client 23 'wy/window-path-to-unix-uri)
          (aset ccls-client 24 'wy/unix-uri-to-windows-path))
      (error "could not find ccls lsp client")))

  ;; -----------------------------------------------------------------------------
  ;; Scheme

  (add-hook 'scheme-mode-hook (lambda ()
                                (paredit-mode 1)))

  ;; -----------------------------------------------------------------------------
  ;; General

  (blink-cursor-mode 0)
  (prefer-coding-system 'utf-8-unix)

  (when (not (display-graphic-p))
    (xterm-mouse-mode 0))

  (setq persp-auto-save-opt 0
        ;; evil-escape-key-sequence "jk"
        haskell-stylish-on-save t ;; override haskell-mode hardcoded
        epg-pinentry-mode 'ask)

  ;; run local hooks
  (run-hooks 'wy/after-config-hook))

;; -----------------------------------------------------------------------------
;; Load vanilla Emacs config if no Spacemacs

(when (not (boundp 'spacemacs-version))
  (load "vanilla"))

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
          (let* ((custom (car (read-from-string (wy/file-string custom-file))))
                 (merged (append custom local-custom)))
            (prin1 merged (current-buffer)))))
      (load generated-custom-file))))

(provide 'willy)
;;; willy.el ends here
