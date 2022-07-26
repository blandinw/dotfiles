;;; -*- lexical-binding: t; -*-
;;; willy --- emacs config
;;; Commentary:
;;; Should work with vanilla, Doom, Spacemacs
;;; Code:
;;;

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
  "`byte-compile' current buffer if it's 'emacs-lisp-mode' and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(defun wy/unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

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

(defun wy/guess-compile-command ()
  (cond
   ((file-exists-p "Makefile") "make -k")

   ((and (eq system-type 'windows-nt)
         (file-exists-p "windows/build.bat"))
    "cmd.exe /c windows\\build.bat")

   ((file-executable-p "build.sh")
    "./build.sh")))

(defun wy/recompute-exec-path ()
  (setq exec-path (split-string (getenv "PATH") path-separator)))

(defun wy/uncapitalize (s)
  (if (> (length s) 0)
      (concat (downcase (substring s 0 1)) (substring s 1))
    s))

(defun wy/list-minor-modes ()
  (interactive)
  (cl-flet ((prettify-mode
             (mode)
             (let* ((sym-name (symbol-name mode))
                    (pretty
                     (if (string-match "\\(\\(-minor\\)?-mode\\)?\\'"
                                       sym-name)
                         (capitalize
                          (substring sym-name
                                     0 (match-beginning 0)))
                       sym-name)))
               (put-text-property 0 (length pretty) 'font-lock-face 'button pretty)
               pretty)))
    (let ((lines (mapcar (lambda (mode)
                           (when (and (boundp mode) (symbol-value mode))
                             (let* ((modef (or (get mode :minor-mode-function) mode))
                                    (short-doc (car (split-string (documentation modef) "\n"))))
                               (list (prettify-mode modef) (wy/uncapitalize short-doc)))))
                         minor-mode-list)))
      (setq lines (seq-filter (lambda (x) x) lines))
      (setq lines (sort lines (lambda (a b) (string-lessp (car a) (car b)))))
      (with-current-buffer-window "wy/list-minor-modes" nil nil
        (dolist (line lines)
          (insert (car line))
          (insert " ")
          (insert (cadr line))
          (insert "\n"))))))

(defun wy/vale ()
  (interactive)
  (compile (format "vale --config %s %s" (expand-file-name "~/dotfiles/vale.ini") buffer-file-name)))

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

     (defun wy/quit-close-reset ()
       (interactive)
       (evil-ex-nohighlight)
       (when (functionp 'pupo/close-window)
         (pupo/close-window))
       (when (functionp '+popup/close-all)
         (+popup/close-all))
       (keyboard-quit))

     (defun wy/tab ()
       (interactive)
       (if (looking-at "$")
           (hippie-expand nil)
         (indent-for-tab-command)))

     (defun wy/evil-define-key-normal* (k f)
       (dolist (m (list evil-normal-state-map evil-motion-state-map))
         (define-key m k f))

       (with-eval-after-load 'evil-evilified-state
         (define-key evil-evilified-state-map-original k f)))

     (with-eval-after-load 'evil
       (evil-mode 1)

       (evil-ex-define-cmd "W" 'evil-write)

       (wy/evil-define-key-normal* (kbd "<escape>") 'wy/quit-close-reset)
       (wy/evil-define-key-normal* (kbd "-") 'evil-window-next)
       (wy/evil-define-key-normal* (kbd "_") 'evil-window-prev)

       (define-key evil-motion-state-map "\\" nil)
       (wy/evil-define-key-normal* (kbd "\\\\") 'evil-execute-in-emacs-state)
       (wy/evil-define-key-normal* (kbd "\\`") 'wy/switch-to-prev-buffer)
       (wy/evil-define-key-normal* "\\b" 'projectile-switch-to-buffer)
       (wy/evil-define-key-normal* "\\m" #'(lambda ()
                                             (interactive)
                                             (require 'mu4e)
                                             (mu4e)))
       (wy/evil-define-key-normal* "\\p" 'projectile-find-file)
       (wy/evil-define-key-normal* "\\s" 'wy/switch-to-or-open-shell)
       (wy/evil-define-key-normal* "\\t" #'(lambda ()
                                             (interactive)
                                             (let ((buf "todo.org"))
                                               (if (buffer-live-p (get-buffer buf))
                                                   (switch-to-buffer buf)
                                                 (message "could not find buffer \"%s\"" buf)))))
       (wy/evil-define-key-normal* "\\ve" #'(lambda ()
                                              (interactive)
                                              (find-library "willy")))
       (wy/evil-define-key-normal* "\\z" 'evil-emacs-state)

       (define-key evil-normal-state-map (kbd "\\c")
         #'(lambda ()
             (interactive)
             (let ((old-dir default-directory)
                   (new-dir (if (fboundp 'projectile-project-root)
                                (projectile-project-root)
                              (file-name-directory buffer-file-name))))
               (cd new-dir)
               (compile (wy/guess-compile-command))
               (cd old-dir))))

       (define-key evil-normal-state-map (kbd "\\g") #'(lambda ()
                                                         (interactive)
                                                         (if (functionp 'wy/debug-function)
                                                             (wy/debug-function)
                                                           (message "No debug function found."))))

       (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
       ;; (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt) ;; clashes with Emacs prefixes
       (define-key evil-normal-state-map (kbd "C-z") 'suspend-frame)
       (define-key evil-insert-state-map (kbd "<enter>") 'evil-ret-and-indent)
       (define-key evil-insert-state-map (kbd "<tab>") 'wy/tab)
       (define-key evil-visual-state-map "u" nil)
       (define-key evil-emacs-state-map (kbd "C-g") 'wy/quit-close-reset))

     ;; -----------------------------------------------------------------------------
     ;; nord-theme.el

     (with-eval-after-load 'nord-theme
       (let ((class '((class color) (min-colors 89)))
             (nord-region-highlight-background (if (string= nord-region-highlight "frost") "#88C0D0"
                                                 (if (string= nord-region-highlight "snowstorm") "#D8DEE9" "#434C5E"))))
         (custom-theme-set-faces
          'nord
          `(org-agenda-dimmed-todo-face ((,class (:foreground ,nord-region-highlight-background))))
          )

         (custom-theme-recalc-face 'org-agenda-dimmed-todo-face)))

     ;; -----------------------------------------------------------------------------
     ;; Doom

     (when (boundp 'doom-version)
       (setq
        ;; doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
        ;; doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13)
        doom-font (font-spec :family "Source Code Variable" :size 13 :weight 'light)
        doom-font (font-spec :family "Cousine" :size 13 :weight 'normal)
        doom-font (font-spec :family "Iosevka Fixed" :size 13 :weight 'normal)
        doom-font (font-spec :family "Fixedsys Excelsior 3.01" :size 15 :weight 'normal)
        ;; doom-font (font-spec :family "Fairfax" :size 16 :weight 'light)
        ;; doom-font (font-spec :family "BigBlue TerminalPlus" :size 12 :weight 'light)
        doom-theme 'doom-nord)

       (define-key! [remap locate] #'consult-find))

     ;; -----------------------------------------------------------------------------
     ;; Projectile

     (with-eval-after-load 'projectile
       (wy/add-to-list* 'projectile-globally-ignored-directories
                        '(".vs" ".vscode" "buck-out" ".ccls-cache")))

     ;; -----------------------------------------------------------------------------
     ;; Flycheck

     (with-eval-after-load 'flycheck
       (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
             flycheck-idle-change-delay 5)
       (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

     ;; -----------------------------------------------------------------------------
     ;; Org-mode: timebox

     (defun wy//time-add-duration-string (point dur)
       ;; e.g.
       ;; 1h
       ;; 1h30min
       ;; 30min
       (let* ((mins (org-duration-to-minutes dur)))
         (time-add point (seconds-to-time (* 60 mins)))))

     (defun wy/timebox-region ()
       ;; e.g.
       ;; 14:00
       ;; 1h30min workout
       ;; 1h lunch break
       ;; 1h admin chore
       ;; 2h coding
       (interactive)
       (pcase-let ((`(,s ,M ,h ,d ,m ,y ,dow ,dst ,utcoff) (decode-time)))
         (let* ((b (region-beginning))
                (e (region-end))
                (text (buffer-substring-no-properties b e))
                (rows (split-string text "\n" t))
                (initial-time (wy//time-add-duration-string
                               (encode-time `(0 0 0 ,d ,m ,y ,dow ,dst ,utcoff))
                               (first rows)))
                (tasks (rest rows)))
           (if (null initial-time)
               (message "error: could not parse region")
             (progn
               (kill-region b e)
               (insert
                (with-temp-buffer
                  (seq-reduce (lambda (acc x)
                                (let* ((x (string-trim x))
                                       (space-idx (string-match " " x))
                                       (range-end (wy//time-add-duration-string acc (substring x 0 space-idx)))
                                       (label (string-trim (substring x space-idx (length x)))))
                                  (insert (format-time-string "%H:%M" acc))
                                  (insert "-")
                                  (insert (format-time-string "%H:%M" range-end))
                                  (insert " ")
                                  (insert label)
                                  (insert "\n")
                                  range-end))
                              tasks initial-time)
                  (buffer-string))))))))

     ;; -----------------------------------------------------------------------------
     ;; Org-mode: MOAR

     (defun wy/helm-link-action-buffer-pos (buffer pos &optional prefix)
       (let* ((prefix (or prefix ""))
              (filename (buffer-file-name buffer))
              (opts (with-current-buffer buffer
                      (save-excursion
                        (goto-char pos)
                        (let ((id (org-id-get-create nil))
                              (item (org-entry-get nil "ITEM")))
                          (list id item)))))
              (id (car opts))
              (item (cadr opts)))
         (insert (format "%sid:%s][%s]]" prefix id item))))

     (defun wy/helm-link-action-marker (marker)
       (let ((buffer (marker-buffer marker))
             (pos (marker-position marker)))
         (wy/helm-link-action-buffer-pos buffer pos)))

     (defun wy/helm-link-action (candidate)
       (let ((buffer (car candidate))
             (pos (cdr candidate)))
         (wy/helm-link-action-buffer-pos buffer pos "[[")))

     (defun wy/org-element-summary ()
       ;; assuming (point) is positioned on desired entry
       (let* ((contents (buffer-substring-no-properties (save-excursion
                                                          (goto-char (org-entry-beginning-position))
                                                          (point))
                                                        (save-excursion
                                                          (goto-char (org-entry-end-position))
                                                          (point)))))
         (org-add-props contents nil 'org-marker (org-agenda-new-marker (point)))
         (list (buffer-file-name) (format "%s" contents))))

     (defun wy/org-fontify (text)
       (with-temp-buffer
         (erase-buffer)
         (insert text)
         (delay-mode-hooks (org-mode))
         (font-lock-default-function 'org-mode)
         (font-lock-default-fontify-region (point-min)
                                           (point-max)
                                           nil)
         (buffer-string)))

     (defun wy/org-moar-toggle-backlinks ()
       (interactive)

       ;; (setq org-ql-cache (make-hash-table :weakness 'key))

       (let ((bufname "*backlinks"))
         (if (get-buffer-window bufname)
             (kill-buffer bufname)
           (let* ((id (org-entry-get nil "ID")))
             (if (not id)
                 (message "could not find current entry id")
               (let* ((backlinks (org-ql-select (org-agenda-files)
                                                `(and (link :target ,(format "id:%s" id)))
                                                :action #'wy/org-element-summary))
                      (buf (get-buffer-create bufname)))

                 (with-current-buffer buf
                   (read-only-mode 0)
                   (erase-buffer)

                   (dolist (x backlinks)
                     (let* ((filename (car x))
                            (title (propertize (abbreviate-file-name filename)
                                               'face 'header-line))
                            (contents (cadr x)))
                       (insert title)
                       (insert "\n")
                       (insert (wy/org-fontify contents))
                       (insert "\n\n")))

                   (read-only-mode)

                   (cl-flet ((k (key func)
                                (local-set-key key func)
                                (when (functionp 'evil-local-set-key)
                                  (evil-local-set-key 'normal key func))))
                     (k "q" #'quit-window)
                     (k (kbd "<tab>") #'org-agenda-goto)
                     (k (kbd "<return>") #'org-agenda-goto)))

                 (display-buffer buf '(nil (inhibit-same-window . t)))))))))

     (defun wy/org-moar-daily-note (do-insert)
       (let ((buf (find-file-noselect wy/org-moar-daily-file)))
         (switch-to-buffer buf)
         (beginning-of-buffer)

         (unless (org-at-heading-p) (outline-next-heading))

         (org-overview)

         (when do-insert
           (let* ((now (current-time))
                  (day (nth 3 (decode-time now)))
                  (new-title (format (format-time-string "%A, %B %-d%%s, %Y")
                                     (case day
                                           ((1 21 31) "st")
                                           ((2 22) "nd")
                                           ((3 23) "rd")
                                           (t "th")))))

             ;; create today's entry if not exist
             (when (or (not (org-at-heading-p))
                       (let* ((headline (org-element-headline-parser (line-end-position)))
                              (plist (cadr headline))
                              (title (plist-get plist :raw-value)))
                         (not (string-equal new-title title))))

               (let ((yday-headings (rest (org-map-entries
                                           (lambda ()
                                             (buffer-substring (save-excursion
                                                                 (org-previous-visible-heading 0)
                                                                 (point))
                                                               (1- (save-excursion
                                                                     (org-end-of-meta-data)
                                                                     (point)))))
                                           nil 'tree))))
                 (org-insert-heading)
                 (insert new-title)
                 (save-excursion
                   (dolist (x yday-headings)
                     (insert "\n")
                     (insert x)))))

             (org-show-subtree)
             (org-end-of-meta-data)
             (goto-char (1- (point)))

             (insert "\n- ")
             (when (boundp 'evil-version)
               (evil-insert 0))))))

     (defun wy/org-moar-create-and-link (name)
       (let* ((buf (find-file-noselect wy/org-moar-file))
              (poi (with-current-buffer buf
                     (save-excursion
                       (beginning-of-buffer)
                       (org-insert-heading)
                       (insert name)
                       (point)))))
         (wy/helm-link-action-buffer-pos buf poi)))

     ;; TODO(willy) fix for doom, does not include helm-org-ql
     (defun wy/org-moar-post-self-insert-hook ()
       (let ((last2 (buffer-substring-no-properties (- (point) 2) (point))))
         (when (string-equal "[[" last2)
           (let ((helm-org-ql-actions #'wy/helm-link-action-marker))
             (cl-flet ((add-dummy-source
                        (src)
                        (list src
                              (helm-make-source "Create new entry" 'helm-source-dummy
                                                :action #'wy/org-moar-create-and-link))))
               (advice-add 'helm-org-ql-source :filter-return #'add-dummy-source)
               (ignore-errors (helm-org-ql-agenda-files))
               (advice-remove 'helm-org-ql-source #'add-dummy-source))))))

     (with-eval-after-load 'helm-org-rifle
       (setq helm-org-rifle-actions
             (append helm-org-rifle-actions
                     (helm-make-actions "Insert link to entry" 'wy/helm-link-action))))

     ;; -----------------------------------------------------------------------------
     ;; Org-mode

     (defun wy/org-projectile-find-project-orgfile (project-path)
       (let ((gitrepo (s-trim (shell-command-to-string "git config --get remote.origin.url"))))
         (cond ((not (string-empty-p gitrepo)) (concat (file-name-base gitrepo) ".org"))
               (t (concat (file-name-base (directory-file-name project-path)) ".org")))))

     (with-eval-after-load 'org
       (setq org-capture-templates '(("j" "Journal entry" entry (file wy/org-journal)
                                      "* %T\n%?")
                                     ("t" "Task" entry (file+headline "" "Tasks")
                                      "* TODO %?\n  %U\n  %a")
                                     ("r" "Roam entry" entry (file wy/org-moar-file)
                                      "* %?\n%U\n%i")
                                     ("c" "Contacts" entry (file wy/org-contacts-file)
                                      "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:"))
             org-refile-targets '((org-agenda-files :maxlevel . 3))
             org-projectile-per-project-filepath 'wy/org-projectile-find-project-orgfile
             org-blank-before-new-entry '((heading) (plain-list-item))
             org-catch-invisible-edits 'smart
             org-contacts-icon-use-gravatar nil
             org-id-link-to-org-use-id t
             org-enforce-todo-dependencies t
             org-agenda-todo-ignore-scheduled 'future
             org-agenda-custom-commands
             '(("n" "Agenda and all TODOs (willy)"
                ((agenda "")
                 (alltodo "" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))))))

       (org-link-set-parameters "open" :follow (lambda (url)
                                                 (shell-command (format "open '%s'" url))))

       (add-hook
        'org-mode-hook
        (lambda ()
          (cond
           ((boundp 'doom-version)
            (map! :map org-mode-map :localleader "aorb" #'wy/org-moar-toggle-backlinks)
            (map! :map org-mode-map :localleader "aord" #'(lambda ()
                                                            (interactive)
                                                            (wy/org-moar-daily-note t)))
            (map! :map org-mode-map :localleader "aort" #'wy/timebox-region))

           ((boundp 'spacemacs-version)
            (spacemacs/set-leader-keys
             "aorb" 'wy/org-moar-toggle-backlinks
             "aord" #'(lambda ()
                        (interactive)
                        (wy/org-moar-daily-note t))
             "aort" 'wy/timebox-region)))

          (when (boundp 'evil-version)
            (evil-local-set-key 'normal
                                (kbd "<s-return>")
                                (lambda ()
                                  (interactive)
                                  (org-open-at-point)
                                  (other-window 1))))

          (add-hook 'post-self-insert-hook #'wy/org-moar-post-self-insert-hook nil t)

          (smartparens-mode 0))))

     ;; -----------------------------------------------------------------------------
     ;; mu4e

     (defun wy/archive-emails-older-than-n-months* (n)
       (let ((prev-headers-append-func mu4e-headers-append-func)
             (prev-found-func mu4e-found-func)
             (msgs nil))

         ;; cf 'mu4e~headers-append-handler'
         (setq mu4e-headers-append-func
               (lambda (msglist)
                 (seq-doseq (x msglist)
                   (push x msgs))))

         ;; cf 'mu4e~headers-found-handler'
         (setq mu4e-found-func
               (lambda (nfound)
                 (let ((msglen (length msgs)))
                   (setq mu4e-headers-append-func prev-headers-append-func
                         mu4e-found-func          prev-found-func)

                   (when (not (= nfound msglen))
                     (error (format "Expected same number for :found (%d) and number of msg S-exps (%d)"
                                    nfound msglen)))

                   (let ((choice (read-char-exclusive (format "Archive %d messages? [y/n]" msglen))))
                     (if (eq choice ?y)
                         (progn
                           (seq-doseq (x msgs)
                             (mu4e--server-move (mu4e-message-field x :docid) mu4e-refile-folder "-N"))
                           (message (format "Archived %d messages." msglen)))
                       (message "Aborted."))))))

         (pcase-let ((`(,s ,M ,h ,d ,m ,y ,dow ,dst ,utcoff) (decode-time)))
           (mu4e--server-find (format "maildir:/%s\/.+/ date:..%s"
                                      wy/maildir-to-archive
                                      (format-time-string "%Y-%m-%d"
                                                          (encode-time s M h d (- m n) y)))
                              nil
                              mu4e-headers-sort-field
                              mu4e-headers-sort-direction
                              nil
                              nil
                              nil))))

     (defun wy/archive-emails-older-than-n-months (n)
       (interactive "nArchive messages older than how many months? ")

       (require 'mu4e)

       (let ((mu4e-debug t)
             (f (lambda () (wy/archive-emails-older-than-n-months* n))))
         (mu4e--init-handlers)
         (if (mu4e-running-p)
             (funcall f)
           (mu4e--start f))))

     (with-eval-after-load 'mu4e
       (setq mu4e-use-maildirs-extension t
             mu4e-enable-notifications t
             mu4e-enable-mode-line t
             ;; INSIDE_EMACS tells pinentry to use emacs' pinentry.el
             mu4e-get-mail-command (format "INSIDE_EMACS=%s mbsync -a" emacs-version)
             mu4e-update-interval ()
             mu4e-compose-signature-auto-include ()
             mu4e-view-show-images t
             mu4e-view-show-addresses t
             mu4e-sent-messages-behavior 'delete
             mu4e-confirm-quit nil
             mu4e-change-filenames-when-moving t
             mu4e-bookmarks
             `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
               ("date:today..now" "Today's messages" ?t)
               ("date:7d..now" "Last 7 days" ?w)
               ("mime:image/*" "Messages with images" ?p))))

     (when (featurep 'make-network-process '(:family local))
       (pinentry-start))

     ;; -----------------------------------------------------------------------------
     ;; Ledger-mode

     (defun wy/ledger-toggle-current-and-go-to-next ()
       (interactive)
       (ledger-toggle-current)
       (ledger-navigate-next-uncleared)
       (evil-scroll-line-to-center nil))

     (with-eval-after-load 'ledger-mode
       (cond
        ((boundp 'doom-version)
         (map! :map ledger-mode-map :localleader "," #'wy/ledger-toggle-current-and-go-to-next))

        ((boundp 'spacemacs-version)
         (spacemacs/set-leader-keys-for-major-mode 'ledger-mode
                                                   "," 'wy/ledger-toggle-current-and-go-to-next))))

     ;; -----------------------------------------------------------------------------
     ;; Emacs Lisp

     (add-hook 'emacs-lisp-mode-hook (lambda ()
                                       (paredit-mode 1)))

     ;; -----------------------------------------------------------------------------
     ;; OCaml

     (add-hook 'tuareg-mode-hook
               (lambda ()
                 ;; locate opam elisp and binaries
                 (let* ((opam-share (car (ignore-errors (process-lines "opam" "config" "var" "share"))))
                        (opam-emacs (format "%s/emacs/site-lisp" opam-share)))
                   (if (file-exists-p opam-emacs)
                       (progn
                         (add-to-list 'load-path opam-emacs)
                         ;; update env with opam vars
                         (let ((pairs-sexp (ignore-errors (process-lines "opam" "config" "env" "--safe" "--sexp"))))
                           (if pairs-sexp
                               (progn
                                 (dolist (x (first (read-from-string (string-join pairs-sexp))))
                                   (setenv (first x) (second x)))
                                 (wy/recompute-exec-path))
                             (message "Aborted. Could not read env vars from opam."))))
                     (message "Aborted. Could not find opam share directory.")))

                 ;; ocamlformat
                 (when (require 'ocamlformat nil 'noerror)
                   (setq ocamlformat-show-errors nil)
                   (add-hook 'before-save-hook #'ocamlformat-before-save))

                 (cond
                  ((boundp 'doom-version)
                   (map! :map tuareg-mode-map :localleader "=" #'ocamlformat))

                  ((boundp 'spacemacs-version)
                   (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode "=" 'ocamlformat))

                  (t
                   (define-key tuareg-mode-map (kbd "=") #'ocamlformat)))))

     ;; -----------------------------------------------------------------------------
     ;; Python

     (with-eval-after-load 'lsp
       (wy/add-to-list* 'lsp-file-watch-ignored
                        ;; exclude not only from topdir
                        '(".*[/\\\\]\\.mypy_cache$")))

     (with-eval-after-load 'python
       )

     ;; -----------------------------------------------------------------------------
     ;; Elixir

     (with-eval-after-load 'lsp
       (wy/add-to-list* 'lsp-file-watch-ignored
                        ;; exclude not only from topdir
                        '(".*[/\\\\]\\.elixir_ls$")))

     ;; -----------------------------------------------------------------------------
     ;; LSP

     (with-eval-after-load 'lsp-mode
       (setq lsp-idle-delay 1
             lsp-headerline-breadcrumb-enable nil
             lsp-enable-dap-auto-configure nil
             ))

     (with-eval-after-load 'lsp-ui
       (setq lsp-ui-doc-delay 1
             lsp-ui-doc-position 'top
             ))

     ;; -----------------------------------------------------------------------------
     ;; DAP

     (with-eval-after-load 'dap-mode
       (setq dap-auto-configure-features '(locals breakpoints expressions tooltip))

       ;; NOTE(willy) ensure these keybinds have precedence by putting them in
       ;; emulation-mode-map-alists. Fix conflict with 'lsp-ui-mode-map'.
       (let ((wy/dap-mode-map (make-sparse-keymap)))
         (define-key wy/dap-mode-map [mouse-movement] 'dap-tooltip-mouse-motion)
         (add-to-list 'emulation-mode-map-alists `((dap-mode . ,wy/dap-mode-map)))))

     (with-eval-after-load 'dap-hydra
       (defhydra+ dap-hydra nil
         "
^Stepping^          ^Switch^                 ^Breakpoints^         ^Debug^                     ^Eval
^^^^^^^^----------------------------------------------------------------------------------------------------------------
_n_: Next           _ss_: Session            _bb_: Toggle          _dd_: Debug                 _ee_: Eval
_i_: Step in        _st_: Thread             _bd_: Delete          _dr_: Debug recent          _er_: Eval region
_o_: Step out       _sf_: Stack frame        _ba_: Add             _dl_: Debug last            _es_: Eval thing at point
_c_: Continue       _sb_: List breakpoints   _bc_: Set condition   _de_: Edit debug template   _ea_: Add expression
_r_: Restart frame  _sl_: List locals        _bh_: Set hit count   _ds_: Debug restart         _ed_: Remove expression
_Q_: Disconnect     _se_: List expressions   _bl_: Set log message
"
         ("se" dap-ui-expressions)
         ("ea" dap-ui-expressions-add-prompt)
         ("ed" dap-ui-expressions-remove)

         ;; see https://github.com/abo-abo/hydra/wiki/internals#cmd=
         ("su" t)
         ("sd" t)
         ))

     ;; -----------------------------------------------------------------------------
     ;; Zig

     (defun wy/make-zig-test-debug-config ()
       (let ((root (projectile-project-root)))
         (let ((test-bin
                (string-trim
                 (car
                  (split-string (shell-command-to-string (format "cd %s && find . -iregex './zig-cache.*test' | xargs ls -t1" root))
                                "\n")))))
           (message (format "Debugging %s" test-bin))
           (list :type "lldb-vscode"
                 :cwd root
                 :request "launch"
                 :args (list "src/main.zig")
                 :program test-bin
                 :name "Zig Tests"))))

     (defun wy/debug-zig ()
       (let* ((fname "wy/zig-lldb")
              (sesh (dap--cur-session))
              ;; NOTE(willy) will be called many times, must be idempotent
              (cleanup-fn
               #'(lambda (sesh)
                   (dap-hydra/nil)
                   (dap-tooltip-activate-mouse-motions nil)
                   (dap-auto-configure-mode -1)
                   (tooltip-mode -1)
                   (let ((debug-frame (condition-case nil
                                          (progn (select-frame-by-name fname)
                                                 (selected-frame))
                                        (error nil))))
                     (when debug-frame
                       (delete-frame debug-frame))))))
         (cond
          (sesh
           (dap-delete-session sesh))
          (t
           (let ((f (make-frame '((undecorated . t)
                                  (fullscreen . fullscreen)))))
             (set-frame-name fname)
             (select-frame f)
             (delete-other-windows)
             (tooltip-mode 1)
             (dap-auto-configure-mode 1)
             (dap-ui-show-many-windows)
             ;; NOTE(willy) deal with timers to properly kill buffers
             ;; without them resurrecting
             (with-current-buffer dap-ui--locals-buffer
               (add-hook 'kill-buffer-hook
                         (lambda ()
                           (when dap-ui--locals-timer
                             (cancel-timer dap-ui--locals-timer)))))
             (with-current-buffer dap-ui--expressions-buffer
               (add-hook 'kill-buffer-hook
                         (lambda ()
                           (when dap-ui--watches-timer
                             (cancel-timer dap-ui--watches-timer)))))
             (dap-tooltip-activate-mouse-motions t)
             (dap-hydra)
             (let ((debug-config (if wy/zig-debug-test-toggle
                                     (wy/make-zig-test-debug-config)
                                   (list :type "lldb-vscode"
                                         :cwd (projectile-project-root)
                                         :request "launch"
                                         :args (list "-Q")
                                         :program "zig-out/bin/main"
                                         :name "Zig LLDB"
                                         :cleanup-function cleanup-fn))))
               (setq debug-config (plist-put debug-config :cleanup-function cleanup-fn))
               (dap-debug debug-config)))))))

     (defun wy/zig-debug-test ()
       (interactive)
       (setq wy/zig-debug-test-toggle (not wy/zig-debug-test-toggle))
       (message (format "Zig debug mode %s" wy/zig-debug-test-toggle)))

     (with-eval-after-load 'zig-mode
       (require 'dap-mode nil 'noerror)
       (setq zig-format-on-save nil)
       (cond
        ((boundp 'doom-version) (map! :localleader
                                      :map zig-mode-map
                                      "d" #'dap-hydra))
        ((boundp 'spacemacs-version) (spacemacs/set-leader-keys "ded" 'dap-ui-expressions-remove))))

     (add-hook
      'zig-mode-hook
      (lambda ()
        (setq compilation-auto-jump-to-first-error t)
        (setq wy/zig-debug-test-toggle nil)

        (when (functionp 'dap-debug)
          (fset 'wy/debug-function #'wy/debug-zig))

        (lsp-deferred)))

     (with-eval-after-load 'lsp-diagnostics
       (add-to-list 'lsp-diagnostics-disabled-modes 'zig-mode))

     ;; -----------------------------------------------------------------------------
     ;; Lua / Fennel

     (when (boundp 'wy/fennel-mode-path)
       (autoload 'fennel-mode wy/fennel-mode-path nil t)
       (add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode)))

     (add-hook 'lua-mode-hook
               (lambda ()
                 (company-mode)
                 (setq-local company-backends '((company-lua
                                                 company-etags
                                                 company-dabbrev-code
                                                 company-yasnippet)))))

     ;; -----------------------------------------------------------------------------
     ;; General

     (prefer-coding-system 'utf-8-unix)

     (blink-cursor-mode -1)

     (global-display-line-numbers-mode -1)
     (remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
       #'display-line-numbers-mode)

     (when (not (display-graphic-p))
       (xterm-mouse-mode -1))

     (when (boundp 'pcre-mode)
       (pcre-mode 1))

     (setq confirm-kill-emacs nil
           epg-pinentry-mode 'ask
           persp-auto-save-opt 0)

     (add-to-list 'image-file-name-extensions "webp")

     ;; Fullscreen mode, non-native
     (set-frame-parameter nil 'undecorated t)
     (toggle-frame-fullscreen)

     (when (and (not (boundp 'spacemacs-version))
                (not (boundp 'doom-version)))
       (load "vanilla"))

     (run-hooks 'wy/after-config-hook))

;; -----------------------------------------------------------------------------
;; Allow local customizations

(when load-file-name
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
        (load generated-custom-file)))))

(provide 'willy)
;;; willy.el ends here
