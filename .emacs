;; begin file

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

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

(load-file "~/.emacs.d/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)

(use-package no-littering)

;; May need to adjust this font size for the system
(defvar jwm/default-font-size 125)

(set-face-attribute 'default nil :font "Fira Code" :height jwm/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height jwm/default-font-size)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Arial Light" :height jwm/default-font-size :weight 'regular)

(global-hl-line-mode t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(use-package diminish)

(setq user-full-name "John Mosty"
      user-mail-address "dubs.mosty@gmail.com"
      calendar-latitude 30.32
      calendar-longitude -97.71
      calendar-location-name "Austin, TX")

(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
               term-mode-hook
               shell-mode-hook
               eshell-mode-hook))
 (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defun hrs/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'hrs/kill-current-buffer)

(defun hrs/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun hrs/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "<f5>") 'org-capture)
(global-set-key (kbd "C-x 2") 'hrs/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'hrs/split-window-right-and-switch)
(global-set-key (kbd "<f4>") 'cfw:open-org-calendar)
(global-set-key (kbd "C-c j") 'org-journal-new-entry)

(set-window-scroll-bars (minibuffer-window) nil nil)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq scroll-conservatively 100)
(setq kill-whole-line t)
(setq kill-read-only-ok t)
(setq require-final-newline t)
(setq echo-keystrokes 0.1)
(setq focus-follows-mouse t)

(setq split-height-threshold nil)
(setq split-width-threshold 0)
(setq org-confirm-elisp-link-function nil)

(setq save-interprogram-paste-before-kill t)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-banner-logo-title "Welcome to the Mac")
(setq dashboard-center-content t)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

(use-package evil
  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil
        evil-want-keybinding nil)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list
        '(ag dired magit mu4e which-key))
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
;;  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)))

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;;(use-package dired-open
;;  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
;;  (setq dired-open-extensions '(("png" . "feh")
;;                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package all-the-icons)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 18)))

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
          modus-themes-completions 'opinionated
          modus-themes-org-habit 'simplified)


  ;; Load the theme files before enabling a theme (else you get an error).
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi);; OR (modus-themes-load-vivendi)
  :bind ("<f12>" . modus-themes-toggle))

;; (use-package doom-themes
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config))

;; (defun transparency (value)
;;   "Sets the transparency of the frame window. 0=transparent/100=opaque."
;;   (interactive "nTransparency Value 0 - 100 opaque:")
;;   (set-frame-parameter (selected-frame) 'alpha value))

;; (defun hrs/apply-theme-drk ()
;;   "Apply my chosen theme and make frames just slightly transparent."
;;   (interactive)
;;   (load-theme 'doom-one t)
;;   (transparency 100))

;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;               (setq doom-modeline-icon t)
;;                 (with-selected-frame frame (hrs/apply-theme-drk))))
;;   (hrs/apply-theme-drk))

;; (setq jwm/themes '(doom-one doom-one-light doom-dark+ doom-challenger-deep doom-acario-light doom-tomorrow-day doom-peacock doom-zenburn doom-gruvbox))
;; (setq jwm/themes-index 0)

;; (defun jwm/cycle-theme ()
;;   (interactive)
;;   (setq jwm/themes-index (% (1+ jwm/themes-index) (length jwm/themes)))
;;   (jwm/load-indexed-theme))

;; (defun jwm/load-indexed-theme ()
;;   (jwm/try-load-theme (nth jwm/themes-index jwm/themes)))

;; (defun jwm/try-load-theme (theme)
;;   (if (ignore-errors (load-theme theme :no-confirm))
;;       (mapcar #'disable-theme (remove theme custom-enabled-themes))
;;     (message "Unable to find theme file for '%s'" theme)))
;; (global-set-key (kbd "<f12>") 'jwm/cycle-theme)

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
  :diminish
  :bind*
  ("C-;" . evil-avy-goto-char-2))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun jwm/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun jwm/org-font-setup ()
  ;; Set faces for heading levels
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Arial" :weight 'regular :height (cdr face)))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . jwm/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (jwm/org-font-setup))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(defun jwm/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
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
          (agenda "" ((org-agenda-ndays 1)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                   (air-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
         ((org-agenda-compact-blocks t)))))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
(define-key global-map "\C-cL" 'org-occur-link-in-agenda-files)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "M-o") 'other-window)

(setq org-capture-templates
      '(("j" "Journal entry" plain (function org-journal-find-location)
         "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
         :jump-to-captured t :immediate-finish t)
        ("t" "Tasks" entry (file+headline "" "Tasks")
         "*** TODO %?\n%U\n %a %i" :prepend t)
        ("T" "Tasks with ClipBoard" entry (file+headline "" "Tasks")
         "*** TODO %?\n%U\n   %^C" :prepend t)))

(use-package org-autolist)
(add-hook 'org-mode-hook (lambda () (org-autolist-mode)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (js . t)))

(setq org-confirm-babel-evaluate nil)
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("js" . "src js"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(use-package calfw)
(use-package calfw-org)

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; orgmode source
    )))

(setq org-journal-dir "~/org/journal/")
(setq org-journal-date-format "%A, %d %B %Y")
(use-package org-journal)

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

(defvar org-journal--date-location-scheduled-time nil)

(defun org-journal-date-location (&optional scheduled-time)
  (let ((scheduled-time (or scheduled-time (org-read-date nil nil nil "Date:"))))
    (setq org-journal--date-location-scheduled-time scheduled-time)
    (org-journal-new-entry t (org-time-string-to-time scheduled-time))
    (unless (eq org-journal-file-type 'daily)
      (org-narrow-to-subtree))
    (goto-char (point-max))))

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

(use-package company)
(global-company-mode)
(global-set-key (kbd "M-/") 'company-complete-common)

(setq ispell-program-name "/usr/local/bin/hunspell")

(require 'ispell)

(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-<f8>") 'flyspell-mode)

(use-package flyspell :demand t
  :config
  (use-package flyspell-correct-ivy)
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

(use-package undo-tree)
(global-undo-tree-mode)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
     (pcase (cons (not (null (executable-find "git")))
                  (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(setq python-shell-intrepreter '("/usr/local/bin/python3"))
(setq org-babel-python-command "/usr/local/bin/python3")

(evil-set-register ?d [?i ?* ?  ?\C-c ?. return escape])
(evil-set-register ?t [?i ?\C-u ?\C-c ?. return escape])

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

  (eshell-git-prompt-use-theme 'git-radar))

(use-package symon)
(add-hook 'after-init-hook 'symon-mode)

(defun buffcop (buffer)
  (with-current-buffer buffer
    (buffer-string)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-export-html-style
   "<style type=\\\"text/css\\\">
  html {
	font-size: 12pt;
  }
  .title { text-align: center; }
  .todo  { color: red; }
  .done { color: green; }
  .timestamp { color: grey }
  .timestamp-kwd { color: CadetBlue }
  .tag { background-color:lightblue; font-weight:normal }
  .target { background-color: lavender; }
  pre {
	border: 1pt solid #AEBDCC;
	background-color: #F3F5F7;
	padding: 5pt;
	font-family: consolas, monospace;
  }
  table { border-collapse: collapse; }
  td, th {
	vertical-align: top;
	<!--border: 1pt solid #ADB9CC;-->
  }
</style>")
 '(org-agenda-files
   '("~/org/habits.org" "~/org/notes.org"))
 '(org-agenda-include-diary t)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-timestamp-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-start-with-follow-mode t)
 '(org-agenda-sticky t)
 '(org-agenda-tags-todo-honor-ignore-options nil)
 '(org-bullets-bullet-list '("►" "▸" "•" "★" "◇" "◇" "◇" "◇") nil nil "Customized with use-package org-bullets")
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
 '(org-todo-keywords '((type "TODO(t)" "STARTED(s!)" "DONE(d!)")))
 '(org-todo-state-tags-triggers nil)
 '(org-use-fast-todo-selection 'auto)
 '(package-selected-packages
   '(modus-themes modus-operandi-theme doom-themes-visual-bell-config doom-themes doom-modeline calfw-org calfw nvm phi-search-dired helpful which-key-posframe all-the-icons-ivy-rich ivy-rich which-key avy diminish ivy dashboard org-bullets use-package helm evil-visual-mark-mode))
 '(pdf-view-midnight-colors (cons "#f8f8f2" "#282a36"))
 '(rustic-ansi-faces
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCDC"])
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