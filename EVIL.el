#+title EVIL_info

* Packages
- Need to M-x package-install use-package-ensure, org-bullets, for cold installs

#+begin_src emacs-lisp
  ;; begin file here
  (set-language-environment "UTF-8")
  (require 'package)
  (require 'org)

  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
  (add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))

  (setq package-enable-at-startup nil)
  (package-initialize)

  (require 'use-package-ensure)
  (setq use-package-always-ensure t)

  (use-package diminish)
  (use-package auto-compile
    :diminish
    :config (auto-compile-on-load-mode))

  (setq load-prefer-newer t)

  (load-file "~/.emacs.d/sensible-defaults.el")
  (sensible-defaults/use-all-settings)
  (sensible-defaults/use-all-keybindings)

  (use-package all-the-icons)
  (use-package no-littering)
  (use-package benchmark-init
    :ensure t
    :config
    ;; To disable collection of benchmark data after init is done.
    (add-hook 'after-init-hook 'benchmark-init/deactivate))

  ;; (require 'server)
  ;; (unless (server-running-p)
  ;;   (cond
  ;;    ((eq system-type 'windows-nt)
  ;;     (setq server-auth-dir "~\\.emacs.d\\server\\"))
  ;;    ((eq system-type 'gnu/linux)
  ;;     (setq server-auth-dir "~/.emacs.d/server/"))
  ;;    ((eq system-type 'Darwin)
  ;;     (setq server-auth-dir "~/.emacs.d/server/")))
  ;;   (setq server-name "emacs-server-file")
  ;;   (server-start))

  (server-start)

  (good-scroll-mode 1)


#+end_src
* Basic setup
** Font setup
- fonts for Windows 10
#+begin_src emacs-lisp
;; May need to adjust this font size for the system
(defvar jwm/default-font-size 125)
(defun jwm/set-font-faces ()
   (message "Setting faces")
   (set-face-attribute 'default nil :font "Inconsolata" :height jwm/default-font-size)
   ;; Set the fixed pitch face
   (set-face-attribute 'fixed-pitch nil :font "Inconsolata" :height jwm/default-font-size)
   ;; Set the variable pitch face
   (set-face-attribute 'variable-pitch nil :font "Arial Light" :height jwm/default-font-size :weight 'regular))

(if (daemonp)
        (add-hook 'after-make-frame-functions
                         (lambda (frame)
                           (with-selected-frame frame
                              (jwm/set-font-faces))))
        (jwm/set-font-faces))

#+end_src
** Basic UI config
#+begin_src emacs-lisp

  ;; Make ESC quit prompts
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  (use-package rainbow-mode
    :diminish)

  (setq user-full-name "John Mosty"
        user-mail-address "jwmosty@ascension.org"
        calendar-latitude 30.32
        calendar-longitude -97.71
        calendar-location-name "Austin, TX")

  (column-number-mode)
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers-type 'visual)
  ;; Disable line numbers for some modes
  (dolist (mode '(term-mode-hook
                  shell-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

  (global-set-key (kbd "<f5>") 'org-capture)
  (global-set-key (kbd "<f4>") 'cfw:open-org-calendar)
  (global-hl-line-mode t)
  (global-set-key (kbd "C-c j") 'org-journal-new-entry)
  (winner-mode)

  (setq default-major-mode 'org-mode)

  (setq indent-tabs-mode nil)
  (setq tab-width 4)

  (use-package eldoc
    :diminish)
#+end_src
** change functions
#+begin_src emacs-lisp
; Re-create ci" ca"...
(defun seek-backward-to-char (chr)
  "Seek backwards to a character"
  (interactive "cSeek back to char: ")
  (while (not (= (char-after) chr))
    (forward-char -1)))

(setq char-pairs
      '(( ?\" . ?\" )
        ( ?\' . ?\' )
        ( ?\( . ?\) )
        ( ?\[ . ?\] )
        ( ?\{ . ?\} )
        ( ?<  . ?>  )))

(defun get-char-pair (chr)
  (let ((result ()))
    (dolist (x char-pairs)
      (setq start (car x))
      (setq end (cdr x))
      (when (or (= chr start) (= chr end))
        (setq result x)))
      result))

(defun get-start-char (chr)
  (car (get-char-pair chr)))
(defun get-end-char (chr)
  (cdr (get-char-pair chr)))

(defun seek-to-matching-char (start end count)
  (while (> count 0)
    (if (= (following-char) end)
        (setq count (- count 1))
      (if (= (following-char) start)
          (setq count (+ count 1))))
    (forward-char 1)))

(defun seek-backward-to-matching-char (start end count)
  (if (= (following-char) end)
      (forward-char -1))
  (while (> count 0)
    (if (= (following-char) start)
        (setq count (- count 1))
      (if (= (following-char) end)
          (setq count (+ count 1))))
    (if (> count 0)
        (forward-char -1))))

(defun delete-between-pair (char)
  "Delete in between the given pair"
  (interactive "cDelete between char: ")
  (seek-backward-to-matching-char (get-start-char char) (get-end-char char) 1)
  (forward-char 1)
  (setq mark (point))
  (seek-to-matching-char (get-start-char char) (get-end-char char) 1)
  (forward-char -1)
  (kill-region mark (point)))

(defun delete-all-pair (char)
  "Delete in between the given pair and the characters"
  (interactive "cDelete all char: ")
  (seek-backward-to-matching-char (get-start-char char) (get-end-char char) 1)
  (setq mark (point))
  (forward-char 1)
  (seek-to-matching-char (get-start-char char) (get-end-char char) 1)
  (kill-region mark (point)))

(global-set-key (kbd "C-c i") 'delete-between-pair)
(global-set-key (kbd "C-c A") 'delete-all-pair)
#+end_src
** UI arrange
#+begin_src emacs-lisp

  (set-window-scroll-bars (minibuffer-window) nil nil)
  (scroll-bar-mode -1)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (setq scroll-conservatively 100)
  (setq kill-whole-line t)
  (setq kill-read-only-ok t)
  (setq require-final-newline t)
  (setq echo-keystrokes 0.1)
  (setq focus-follows-mouse t)
  (setq split-height-threshold nil)
  (setq split-width-threshold 100)
  (setq save-interprogram-paste-before-kill t)

  (mouse-avoidance-mode 'exile)  ;; jump to corner when approached

  (defun my-pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (dolist (command '(recenter-top-bottom other-window ace-window my-scroll-down-half my-scroll-up-half))
    (advice-add command :after #'my-pulse-line))

#+end_src
** Backups
#+begin_src emacs-lisp
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
#+end_src
* Evil mode
#+begin_src emacs-lisp
  (use-package evil
    :init
    (setq evil-want-abbrev-expand-on-insert-exit nil
          evil-want-keybinding nil)
    :config
    (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)
    (evil-mode 1)
    (defalias 'evil-insert-state 'evil-emacs-state))

  (use-package evil-collection
    :after evil
    :config
    (setq evil-collection-mode-list
          '(ag dired magit mu4e which-key))
    (evil-collection-init)
    :custom
    (evil-collection-setup-minibuffer t))

  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))

  (use-package evil-org
    :after org
    :diminish
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
              (lambda () (evil-org-set-key-theme)))
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))

  (defun add-to-map(keys func)
    "Add a keybinding in evil mode from keys to func."
    (define-key evil-normal-state-map (kbd keys) func)
    (define-key evil-motion-state-map (kbd keys) func))

  (add-to-map "<SPC>" nil)

  (add-to-map "<SPC> f" 'find-file)
  (add-to-map "<SPC> s" 'save-buffer)
  (add-to-map "<SPC> b" 'switch-to-buffer)

  (defun jwm/kill-current-buffer ()
    "Kill the current buffer without prompting."
    (interactive)
    (kill-buffer (current-buffer)))

  (add-to-map "<SPC> k" 'jwm/kill-current-buffer)

  (global-set-key (kbd "C-x k") 'jwm/kill-current-buffer)
#+end_src
* Dired
#+begin_src emacs-lisp
  (use-package dired
    :ensure nil
    :commands (dired dired-jump)
    :bind (("C-x C-j" . dired-jump))
    :custom ((dired-listing-switches "-Aht"))
    :config
    (evil-collection-define-key 'normal 'dired-mode-map
      "h" 'dired-single-up-directory
      "l" 'dired-single-buffer))

   ;; (add-hook 'dired-mode-hook
        ;; (lambda ()
          ;; (dired-hide-details-mode)))

  (add-hook 'dired-mode-hook 'treemacs-icons-dired-mode)

  (use-package dired-single)

  (add-to-map "<SPC> d" 'dired)

  (add-to-map "<SPC> j" 'dired-jump)
  ;;(use-package dired-open
  ;;  :config
    ;; Doesn't work as expected!
    ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  ;;  (setq dired-open-extensions '(("png" . "feh")
  ;;                                ("mkv" . "mpv"))))

  ;; (use-package dired-hide-dotfiles
    ;; :hook (dired-mode . dired-hide-dotfiles-mode)
    ;; :config
    ;; (evil-collection-define-key 'normal 'dired-mode-map
      ;; "H" 'dired-hide-dotfiles-mode))

#+end_src

* Theme config
#+begin_src emacs-lisp
     ;; (use-package doom-modeline
     ;;   :init (doom-modeline-mode 1)
     ;;   :custom ((doom-modeline-height 35)))
     ;;  (setq doom-modeline-icon (display-graphic-p))
     ;;  (setq doom-modeline-window-width-limit fill-column)

      (use-package modus-themes
        :ensure                         ; omit this to use the built-in themes
        :init
        ;; Add all your customizations prior to loading the themes
        (setq modus-themes-slanted-constructs t
              modus-themes-bold-constructs t
              modus-themes-intense-hl-line t
              modus-themes-subtle-line-numbers t
              modus-themes-links 'faint-neutral-underline
              modus-themes-syntax 'faint
              modus-themes-mode-line 'moody
              modus-themes-completions 'opinionated
              modus-themes-org-habit 'simplified)

        ;; Load the theme files before enabling a theme (else you get an error).
        (modus-themes-load-themes)
        :config
        ;; Load the theme of your choice:
        (modus-themes-load-operandi);; OR (modus-themes-load-vivendi)
        :bind ("<f12>" . modus-themes-toggle))

      (use-package doom-themes
         :config
    ;;     ;; Global settings (defaults)
    ;;     (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
    ;;           doom-themes-enable-italic t) ; if nil, italics is universally disabled
    ;;     ;; Enable flashing mode-line on errors
         (doom-themes-visual-bell-config))

       (defun transparency (value)
         "Sets the transparency of the frame window. 0=transparent/100=opaque."
         (interactive "nTransparency Value 0 - 100 opaque:")
         (set-frame-parameter (selected-frame) 'alpha value))

     (defun hrs/apply-theme-drk ()
       "Apply my chosen theme and make frames just slightly transparent."
       (interactive)
       (modus-themes-load-operandi)
       (transparency 95))

     (if (daemonp)
         (add-hook 'after-make-frame-functions
                   (lambda (frame)
                     (with-selected-frame frame (hrs/apply-theme-drk))))
       (hrs/apply-theme-drk))

    ;;   (setq jwm/themes '(doom-one-light doom-one doom-acario-light doom-zenburn doom-molokai doom-vibrant doom-dark+ doom-dracula doom-fairy-floss doom-gruvbox doom-material doom-nord doom-nord-light doom-nova doom-opera doom-opera-light doom-palenight doom-snazzy doom-sourcerer doom-spacegrey doom-tomorrow-night doom-tomorrow-day))
    ;;   (setq jwm/themes-index 0)

    ;;   (defun jwm/cycle-theme ()
    ;;     (interactive)
    ;;     (setq jwm/themes-index (% (1+ jwm/themes-index) (length jwm/themes)))
    ;;     (jwm/load-indexed-theme))

    ;;   (defun jwm/load-indexed-theme ()
    ;;     (jwm/try-load-theme (nth jwm/themes-index jwm/themes)))

    ;;   (defun jwm/try-load-theme (theme)
    ;;     (if (ignore-errors (load-theme theme :no-confirm))
    ;;         (mapcar #'disable-theme (remove theme custom-enabled-themes))
    ;;       (message "Theme '%s' loaded" theme)))
    ;;   (global-set-key (kbd "<f12>") 'jwm/cycle-theme)

#+end_src
* Ivy hydra Prescient
#+begin_src emacs-lisp

    (use-package hydra)

    (use-package ivy-hydra
      :defer t
      :after hydra)

    (use-package ivy
      :diminish
      :config
      (ivy-mode 1))

    (use-package counsel
      :bind
      (("M-y" . counsel-yank-pop)
       :map ivy-minibuffer-map
       ("M-y" . ivy-next-line)))

    (use-package swiper)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    ;; enable this if you want `swiper' to use it
    ;; (setq search-default-mode #'char-fold-to-regexp)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

    (use-package ivy-rich
     :init
     (ivy-rich-mode 1))

  (use-package avy
    :ensure t
    :diminish
    :bind ("M-s" . avy-goto-word-1))

    (use-package which-key
      :init (which-key-mode)
      :diminish which-key-mode
      :config
      (setq which-key-idle-delay 0.3))

  (use-package prescient)
  (use-package ivy-prescient
    :config
    (ivy-prescient-mode 1))
  (use-package company-prescient
    :config
    (company-prescient-mode 1))

#+end_src
* Helpful
#+begin_src emacs-lisp
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

#+end_src
* Projectile Magit
#+begin_src emacs-lisp
  (use-package projectile
    :diminish projectile-mode
    :config (projectile-mode)
    :custom ((projectile-completion-system 'ivy))
    :bind-keymap
    ("C-c p" . projectile-command-map)
    :init
    ;; NOTE: Set this to the folder where you keep your Git repos!
    (when (file-directory-p "c:/users/jwmosty/AppData/Roaming/GAS_Asc")
      (setq projectile-project-search-path '("c:/users/jwmosty/AppData/Roaming/GAS_Asc")))
    (setq projectile-switch-project-action #'projectile-dired))

  (use-package counsel-projectile
    :config (counsel-projectile-mode))

  (use-package magit
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    (setq  magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
           magit-push-always-verify nil))

  (add-to-map "<SPC> m" 'magit-status)

#+end_src
* Org Mode
** Main Org-mode
#+begin_src emacs-lisp
  (defun jwm/org-mode-setup ()
    (org-indent-mode)
    (visual-line-mode 1)
    (diminish 'visual-line-mode)
    (diminish 'org-indent-mode))

  (defun jwm/org-font-setup ()
    ;; Replace list hyphen with dot
    ;; Set faces for heading levels
    (dolist (face '((org-level-1 . 1.2)
                    (org-level-2 . 1.1)
                    (org-level-3 . 1.05)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font "Bahnschrift" :weight 'regular :height (cdr face)))

    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil :foreground nil :font "Inconsolata" :height 145)

    (set-face-attribute 'fixed-pitch nil :font "Inconsolata" :height 145)
    (set-face-attribute 'org-code nil   :font "Inconsolata" :height 145)
    (set-face-attribute 'org-table nil   :font "Inconsolata" :height 145)
    (set-face-attribute 'org-verbatim nil :font "Inconsolata" :height 145)
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :font "Inconsolata" :height 145))

  (use-package org
    :hook (org-mode . jwm/org-mode-setup)
    :config
    (setq org-ellipsis " ▾")
    ;;(jwm/org-font-setup)
    (setq org-adapt-indentation nil))

  (add-hook 'before-save-hook 'time-stamp)

  ;;(require 'org-bullets)
  ;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

  (require 'org-superstar)
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

  (setq inhibit-compacting-font-caches t)

  (defun jwm/org-mode-visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  (use-package visual-fill-column
    :diminish
    :hook (org-mode . jwm/org-mode-visual-fill))

  (setq org-default-notes-file (concat org-directory "/notes.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (defun air-org-skip-subtree-if-habit ()
    "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (string= (org-entry-get nil "STYLE") "habit")
          subtree-end
        nil)))

  (defun air-org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.

           PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
        nil)))

  (setq org-agenda-custom-commands
        '(("d" "Daily agenda and all TODOs"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))
            (agenda "" ((org-agenda-ndays 2)))
            (alltodo ""
                     ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                     (air-org-skip-subtree-if-priority ?A)
                                                     (org-agenda-skip-if nil '(scheduled deadline))))
                      (org-agenda-overriding-header "ALL normal priority tasks:"))))
           ((org-agenda-compact-blocks t)))))

  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key "\C-cl" 'org-store-link)
  (define-key global-map "\C-cL" 'org-occur-link-in-agenda-files)
  (global-set-key (kbd "<home>") 'begsinning-of-buffer)
  (global-set-key (kbd "M-o") 'other-window)

  (setq org-agenda-follow-mode nil)

  (setq org-agenda-files (directory-files-recursively "d:/My Drive/Org_Files/" "\\.org$"))

  (defun renewOrgBuffer ()
    (interactive)
    (setq org-agenda-files (directory-files-recursively "d:/My Drive/Org_Files/" "\\.org$"))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'org-agenda-mode)
          (org-agenda-maybe-redo))))
    )

  (add-to-map "<SPC> r" 'renewOrgBuffer)

  ;;   (run-with-idle-timer 3 1000 #'renewOrgBuffer)

  (add-to-list 'auto-mode-alist '("README$" . org-mode))

  (require 'org-web-tools)

  (defun toggle-html-export-on-save ()
    "Enable or disable export HTML when saving current buffer."
    (interactive)
    (when (not (eq major-mode 'org-mode))
      (error "Not an org-mode file!"))
    (if (memq 'org-html-export-to-html after-save-hook)
        (progn (remove-hook 'after-save-hook 'org-html-export-to-html t)
               (message "Disabled org html export on save"))
      (add-hook 'after-save-hook 'org-html-export-to-html nil t)
      (set-buffer-modified-p t)
      (message "Enabled org html export on save")))

  (defun org-web-tools-insert-link-for-clipboard-url ()
    "Extend =org-web-tools-inster-link-for-url= to take URL from clipboard or kill-ring"
    (interactive)
    (org-web-tools--org-link-for-url (org-web-tools--get-first-url)))

  (defun org-web-tools-insert-link-for-given-url ()
    "Extend =org-web-tools-inster-link-for-url= to take a user given URL"
    (interactive)
    (let ((url (read-string "Link: ")))
      (org-web-tools--org-link-for-url url)))

  (setq org-capture-templates
        '(("b" "Bookmark (Clipboard)" entry
           (file+headline "~/org/links.org" "INBOX")
           "** %(org-web-tools-insert-link-for-clipboard-url)\n:PROPERTIES:\n:TIMESTAMP: %t\n:END:%?\n" :empty-lines 1 :prepend t)
          ("B" "Bookmark (Paste)" entry
           (file+headline "~/org/links.org" "INBOX")
           "** %(org-web-tools-insert-link-for-given-url)\n:PROPERTIES:\n:TIMESTAMP: %t\n:END:%?\n" :empty-lines 1 :prepend t)
          ("t" "Tasks" entry (file+headline "" "Tasks")
           "*** TODO %?\n%U\n %a %i" :prepend t)
          ("T" "Tasks with ClipBoard" entry (file+headline "" "Tasks")
           "*** TODO %?\n%U\n   %^C" :prepend t)))

  (setq org-default-notes-file "d:/My Drive/Org_Files/notes.org")
  (setq org-duration-format 'h:mm)
  (setq org-confirm-elisp-link-function nil)

  (use-package org-autolist
    :diminish)

  (add-hook 'org-mode-hook (lambda () (org-autolist-mode)))

  (set-face-attribute 'org-headline-done nil :strike-through t)

#+end_src
** Babel and Structure templates
#+begin_src emacs-lisp
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (js . t)
     (ledger . t)
     (kotlin . t)))

  (setq org-confirm-babel-evaluate nil)
  (require 'org-tempo)

  (require 'ob-kotlin)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("js" . "src js"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("ko" . "src kotlin"))
  (add-to-list 'org-structure-template-alist '("le" . "src ledger"))

#+end_src
** LaTeX
#+begin_src emacs-lisp
  (require 'ox-latex)
  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil))
  (add-to-list 'org-latex-classes
               `("article"
                 "\\documentclass{article}"
                 ("\\section{%s}" . "\\section*{%s}")))

  (require 'ox-html)
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
#+end_src
** Encryption
#+begin_src emacs-lisp
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))

  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key '("0F5CDB0D40E4D8AF93DE2C70D5E19C8A72EAD74F"))
  ;;(setq org-crypt-key nil)
  (setq auto-save-default nil)

  (add-to-map "<SPC> u" 'org-decrypt-entry)
  (global-set-key (kbd "C-c e") 'org-decrypt-entry)
#+end_src
* Calculator
#+begin_src emacs-lisp
(defun my-calc-line (arg)
  "Evaluate calc expression in the current line and display the
result in the echo area by skipping everything after the final
'=' sign.

With prefix ARG non-nil or repeating command interactively,
insert the result at the end of line & add a space if necessary
for delimiting clearing everything after '=' sign if it is here."
  (interactive "P")
  (require 'calc)
  (save-excursion
    (let (beg end expr result)
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (search-backward "=" beg t)
      (setq end (point))
      (setq expr (buffer-substring-no-properties beg end))
      (setq result (calc-eval expr))
      (if (and (null arg) (not (eq 'my-calc-line last-command)))
          (message "%s = %s" expr result)
        (end-of-line)
        (setq end (point))
        (when (search-backward "=" beg t)
          (forward-char 1)
          (delete-region (point) end))
        (unless (eq (char-before) ?\ )
          (insert ?\ ))
        (insert result)))))

    (add-to-map "<SPC> =" 'my-calc-line)
#+end_src
* Presentation
#+begin_src emacs-lisp
(use-package hide-mode-line)

(defun jwm/presentation-setup ()
  ;; Hide the mode line
  (hide-mode-line-mode 1)

  ;; Display images inline
  (org-display-inline-images) ;; Can also use org-startup-with-inline-images

  ;; Scale the text.  The next line is for basic scaling:
  (setq text-scale-mode-amount 3)
  (text-scale-mode 1)
  (blink-cursor-mode -1))

  ;; This option is more advanced, allows you to scale other faces too
  ;; (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
  ;;                                    (org-verbatim (:height 1.75) org-verbatim)
  ;;                                    (org-block (:height 1.25) org-block))))

(defun jwm/presentation-end ()
  ;; Show the mode line again
  (hide-mode-line-mode 0)

  ;; Turn off text scale mode (or use the next line if you didn't use text-scale-mode)
 (text-scale-mode 0)
 (blink-cursor-mode 1))

  ;; If you use face-remapping-alist, this clears the scaling:
  ;; (setq-local face-remapping-alist '((default variable-pitch default))))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . jwm/presentation-setup)
         (org-tree-slide-stop . jwm/presentation-end))
  :custom
  (org-tree-slide-slide-in-effect t)
  (org-tree-slide-activate-message "Presentation started")
  (org-tree-slide-deactivate-message "Presentation finished")
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " //  ")
  (org-image-actual-width nil))
#+end_src
* Spell
#+begin_src emacs-lisp

    (use-package company
      :diminish
      :config
      (setq company-idle-delay 0.5)
      (setq company-minimum-prefix-length 1)
      (add-hook 'after-init-hook 'global-company-mode))

    (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)

    (setq ispell-program-name "C:/Users/jwmosty/AppData/Roaming/hunspell-bin/bin/hunspell.exe")

    (require 'ispell)

    (global-set-key (kbd "<f8>") 'ispell-word)
    (global-set-key (kbd "C-<f8>") 'flyspell-mode)

    (use-package flyspell
      :demand t
      :diminish
      :config
      (use-package flyspell-correct-ivy
        :diminish)
      (defun flyspellCompletion()
        (flyspell-mode 1)
        (set (make-local-variable 'company-backends)
             (copy-tree company-backends))
        (add-to-list 'company-backends 'company-ispell))
      (defun flyspell-most-modes()
        (add-hook 'text-mode-hook 'flyspellCompletion)
        (add-hook 'prog-mode-hook 'flyspellCompletion)
        (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
          (add-hook hook (lambda ()
                           (flyspell-mode -1)))))
      (flyspell-most-modes)
      :bind (:map flyspell-mode-map
                  ("C-." . flyspell-correct-wrapper)))

#+end_src
* Dashboard
#+begin_src emacs-lisp
  (defun my/dashboard-banner ()
    """Set a dashboard banner including information on package initialization
             time and garbage collections."""
    (setq dashboard-banner-logo-title
          (format "Emacs ready in %.2f seconds with %d garbage collections."
                  (float-time (time-subtract after-init-time before-init-time)) gcs-done)))

  (use-package dashboard
    :ensure t
    ;; :init
    ;; (add-hook 'after-init-hook 'dashboard-refresh-buffer)
    ;;    (add-hook 'dashboard-mode-hook 'my/dashboard-banner)
    :diminish
    :config
    (dashboard-setup-startup-hook)
    (setq dashboard-center-content t)
    (setq dashboard-startup-banner 6)
    (setq dashboard-banner-logo-title "Welcome to the Work computer")
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-navigator t)
    (setq dashboard-items '((recents  . 5)
                            (bookmarks . 5)
                            (agenda . 10)
                            (registers . 5))))
  ;;:hook
  ;;((dashboard-mode . my/dashboard-banner)))

  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

  (diminish 'page-break-lines-mode)
#+end_src
* Macros and registers
#+begin_src emacs-lisp
  (evil-set-register ?d [?i ?* ?  ?\C-c ?. return escape])
  (evil-set-register ?t [?i ?\C-u ?\C-c ?. return escape])
  (evil-set-register ?b [?$ ?0 ?i ?+ escape ?A ?+ S-right escape])

  (add-to-map "Y" 'append-to-register)
  (add-to-map "P" 'insert-register)

  (setq register-separator ?+)
  (set-register register-separator "\n\n")

#+end_src
* Language
#+begin_src emacs-lisp
  (use-package lsp-mode
    :diminish

    :init
    ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
    (setq lsp-keymap-prefix "C-l")
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
           (python-mode . lsp)
           ;; if you want which-key integration
           (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp)

  (use-package lsp-ui
    :diminish
    :hook (lsp-mode . lsp-ui-mode)
    :custom
    (lsp-ui-doc-position 'bottom))
  (use-package lsp-ivy
    :diminish
    )


#+end_src
* Python
#+begin_src emacs-lisp

  (setq org-babel-python-command "c:/Users/jwmosty/AppData/Local/Programs/Python/Python38-32/python.exe")
  (setq python-shell-interpreter "c:/Users/jwmosty/AppData/Local/Programs/Python/Python38-32/python.exe")
#+end_src
* eshell
#+begin_src emacs-lisp
    (defun jwm/configure-eshell ()
      ;; Save command history when commands are entered
      (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

      ;; Truncate buffer for performance
      (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

      ;; Bind some useful keys for evil-mode
      (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
      (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
      (evil-normalize-keymaps)

      (setq eshell-history-size         10000
            eshell-buffer-maximum-lines 10000
            eshell-hist-ignoredups t
            eshell-scroll-to-bottom-on-input t))

    (use-package eshell-git-prompt)

    (use-package eshell
      :hook (eshell-first-time-mode . jwm/configure-eshell)
      :config

      (with-eval-after-load 'esh-opt
        (setq eshell-destroy-buffer-when-process-dies t)
        (setq eshell-visual-commands '("htop" "zsh" "vim")))

      (eshell-git-prompt-use-theme 'robbyrussell))

    (use-package symon)
    (add-hook 'after-init-hook 'symon-mode)

    (defun buffcop (buffer)
      (with-current-buffer buffer
        (buffer-string)))

    (defun eshell/emacs (file)
        (find-file file))

    (defun eshell/ffo (file)
        (find-file-other-frame file))

  (global-set-key (kbd "<C-return>") 'eshell)

#+end_src
* Popper
#+begin_src emacs-lisp
  (use-package popper
    :diminish
    :ensure t
    :bind (("C-`"   . popper-toggle-latest)
           ("M-`"   . popper-cycle)
           ("C-M-`" . popper-toggle-type)
           ("M-_"   . popper-lower-to-popup)
           ("M-^"   . popper-raise-popup))

    :init
    (setq popper-reference-buffers
          '("^\\*Messages\\*"
            "^Output\\*"
            "^\\Calc:"
            "^\\Warnings\\*"
            help-mode
            Helpful-mode
            compilation-mode
            messages-mode
            occur-mode))
    (setq popper-display-function #'popper-select-popup-at-bottom)
    (popper-mode +1))
#+end_src
* Ledger
#+begin_src emacs-lisp

  (use-package ledger-mode
      :ensure t
      :init
      (setq ledger-clear-whole-transactions 1)
      :config
      (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
      :mode "\\.dat//'")

    (add-hook 'ledger-mode-hook
            (lambda ()
                (setq-local tab-always-indent 'complete)
                (setq-local completion-cycle-threshold t)
                (setq-local ledger-complete-in-steps t)))
#+end_src
* Custom variable
#+begin_src emacs-lisp
   (custom-set-variables
    ;; custom-set-variables was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.
    '(org-agenda-include-diary t)
    '(org-agenda-skip-deadline-if-done t)
    '(org-agenda-skip-scheduled-if-done t)
    '(org-agenda-skip-timestamp-if-done t)
    '(org-agenda-start-on-weekday nil)
    '(org-agenda-sticky t)
    '(org-agenda-tags-todo-honor-ignore-options nil)
    '(org-clock-into-drawer "LOGBOOK")
    '(org-closed-keep-when-no-todo nil)
    '(org-enforce-todo-checkbox-dependencies t)
    '(org-enforce-todo-dependencies t)
    '(org-hide-emphasis-markers t)
    '(org-log-done 'time)
    '(org-log-done-with-time t)
    '(org-log-into-drawer t)
    '(org-return-follows-link t)
    '(org-special-ctrl-a/e t)
    '(org-special-ctrl-k t)
    '(org-todo-keywords '((type "TODO(t)" "DONE(d!)")))
    '(org-todo-state-tags-triggers nil)
    '(org-use-fast-todo-selection 'auto)
    '(package-selected-packages
      '(doom-themes-visual-bell-config doom-themes phi-search-dired helpful which-key-posframe all-the-icons-ivy-rich ivy-rich which-key avy diminish ivy dashboard org-bullets use-package evil-visual-mark-mode))
    '(pdf-view-midnight-colors (cons "#f8f8f2" "#282a36"))
    '(rustic-ansi-faces
      ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCDC"])
    '(safe-local-variable-values
        '((eval add-hook 'after-save-hook 'org-html-export-to-html t t)
        (eval add-hook 'after-save-hook #'org-babel-tangle t t)
        (org-confirm-babel-evaluate)))
    '(tab-bar-mode t)
    '(tool-bar-mode nil)
    '(vc-annotate-background nil)
    '(vc-annotate-background-mode nil))
   (custom-set-faces
    ;; custom-set-faces was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.
    )
#+end_src
