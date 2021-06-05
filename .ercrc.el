;;; ERC configuration

;; Load authentication info from an external source.  Put sensitive
;; passwords and the like in here.
;; (load "~/.authinfo.gpg")

(use-package erc
  :custom
  (erc-autojoin-channels-alist '(("libera.chat" "#libera"
                                  "#emacs" "#archlinux" "#manjaro" "#systemcrafters")))
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 7)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-prompt-for-nickserv-password nil)
  (erc-lurker-threshold-time 43200)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :config
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-services-mode 1)
  (erc-update-modules))
;; This is an example of how to make a new command.  Type "/uptime" to
;; use it.
(defun erc-cmd-UPTIME (&rest ignore)
  "Display the uptime of the system, as well as some load-related
stuff, to the current ERC buffer."
  (let ((uname-output
         (replace-regexp-in-string
          ", load average: " "] {Load average} ["
          ;; Collapse spaces, remove
          (replace-regexp-in-string
           " +" " "
           ;; Remove beginning and trailing whitespace
           (replace-regexp-in-string
            "^ +\\|[ \n]+$" ""
            (shell-command-to-string "uptime"))))))
    (erc-send-message
     (concat "{Uptime} [" uname-output "]"))))

;; This causes ERC to connect to the Libra network upon hitting
;; C-c f.
(defun jwm/ERC-load ()
  (interactive)
  (erc-tls
   :server "irc.libera.chat"
   :port "6697"
   :nick "dubs"))
(global-set-key (kbd "C-c f") 'jwm/ERC-load)

;; This causes ERC to connect to the IRC server on your own machine (if
;; you have one) upon hitting C-c e b.  Replace MYNICK with your IRC
;; nick.  Often, people like to run bitlbee (https://bitlbee.org/) as an
;; AIM/Jabber/MSN to IRC gateway, so that they can use ERC to chat with
;; people on those networks.
;; (global-set-key "\C-ceb" (lambda () (interactive)
;;                           (erc :server "localhost" :port "6667"
;;                                :nick "MYNICK")))

;; Make C-c RET (or C-c C-RET) send messages instead of RET.  This has
;; been commented out to avoid confusing new users.
;; (define-key erc-mode-map (kbd "RET") nil)
;; (define-key erc-mode-map (kbd "C-c RET") 'erc-send-current-line)
 (define-key erc-mode-map (kbd "C-c C-RET") 'erc-send-current-line)

;;; Options

;; Join the #emacs and #erc channels whenever connecting to Freenode.
;;(setq erc-autojoin-channels-alist '(("#libera" "#emacs" "#org-mode")))

;; Rename server buffers to reflect the current network name instead
;; of SERVER:PORT (e.g., "freenode" instead of "irc.freenode.net:6667").
;; This is useful when using a bouncer like ZNC where you have multiple
;; connections to the same server.
(setq erc-rename-buffers t)

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

;; The following are commented out by default, but users of other
;; non-Emacs IRC clients might find them useful.
;; Kill buffers for channels after /part
 (setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
 (setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
 (setq erc-kill-server-buffer-on-quit t)

(setq erc-log-channels-directory "~/.emacs.d/logs/")
(setq erc-save-buffer-on-part t)

(use-package erc-hl-nicks
  :after erc
  :config
  (add-to-list 'erc-modules 'hl-nicks))

(use-package erc-image
  :ensure t
  :after erc
  :config
  (setq erc-image-inline-rescale 25)
  (add-to-list 'erc-modules 'image))

(use-package emojify
  :ensure t
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

(defun jwm/erc-count-users ()
  "Displays the number of users connected on the current channel."
  (interactive)
  (if (get-buffer "irc.libera.chat:6697")
      (let ((channel (erc-default-target)))
        (if (and channel (erc-channel-p channel))
            (message "%d users are online on %s"
                     (hash-table-count erc-channel-users)
                     channel)
          (user-error "The current buffer is not a channel")))
    (user-error "You must first start ERC")))

 (defun my-erc-generate-log-file-name-short (buffer &optional target
          nick server port)
   "This function uses the buffer-name as a file, with some replacing."
   (let* ((name (buffer-name buffer))
   (name (replace-regexp-in-string "|" "-" name)))
     (concat erc-log-channels-directory "/" name ".txt")))

;; (setq erc-generate-log-file-name-function 'my-erc-generate-log-file-name-short)

 (defun erc-save-buffers-in-logs ()
   (interactive)
   (mapc (lambda(buf)
    (save-excursion
      (set-buffer buf)
      (erc-save-buffer-in-logs)))
  (erc-buffer-filter (lambda() t))))

 (defadvice save-buffers-kill-emacs
   (before save-logs-before-save-buffers-kill-emacs (&rest args) activate)
   'erc-save-buffers-in-logs)

 (defadvice save-some-buffers
   (before save-logs-before-save-some-buffers (&rest args) activate)
   'erc-save-buffers-in-logs)
