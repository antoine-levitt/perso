(require 'erc)
;;--------------------
;; Helper functions
;;--------------------

;;notification
(setq do-not-disturb nil)
;;set this if you don't want to be disturbed by notifications
;;(setq do-not-disturb t)
(defun notify (title message)
  "Notify user by graphical display"
  (unless do-not-disturb
    (shell-command-to-string (format
			      "notify-send %s %s"
			      (shell-quote-argument (concat "" title))
			      (shell-quote-argument (concat "" message))))))

;;ERC tray. Needs tray_daemon, http://smeuuh.free.fr/tray_daemon/
;;defined in emacs_perso : list of regexps for which we don't blink
;;the tray icon
(setq erc-tray-inhibit-one-activation nil)
(setq erc-tray-ignored-channels nil)
(setq erc-tray-state nil)
(defun erc-tray-change-state-aux (arg)
  "Enables or disable blinking, depending on arg (non-nil or nil)"
  (unless (eq erc-tray-state arg)
    (shell-command-to-string
     (concat "echo " (if arg "B" "b") " > /tmp/tray_daemon_control"))
    (setq erc-tray-state arg)))
(defun erc-tray-change-state (arg)
  "Enables or disable blinking, depending on arg (t or nil).
Additional support for inhibiting one activation (quick hack)"
  (if erc-tray-inhibit-one-activation
      (setq erc-tray-inhibit-one-activation nil)
    (erc-tray-change-state-aux arg)))

;;--------------------
;;Settings
;;--------------------
(setq erc-modules '(autojoin button completion irccontrols list
			     log match menu move-to-prompt
			     netsplit networks noncommands
			     readonly ring scrolltobottom
			     services stamp track smiley
			     autoaway truncate))
;;301 : "x is away"
;;305 306 : away messages
(setq erc-hide-list '("301" "305" "306")
      erc-server-reconnect-attempts t
      erc-prompt ">"
      erc-minibuffer-ignored t
      erc-query-display 'buffer
      erc-auto-query 'bury
      erc-current-nick-highlight-type 'all
      erc-interpret-mirc-color t
      erc-log-channels-directory "~/.erclogs"
      erc-log-write-after-insert t
      erc-log-write-after-send t
      erc-log-file-coding-system 'utf-8
      erc-generate-log-file-name-function 'erc-generate-log-file-name-short
      erc-prompt-for-nickserv-password nil
      erc-track-enable-keybindings nil
      erc-track-exclude-server-buffer t
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "305" "306" "333" "353")
      erc-track-position-in-mode-line t
      erc-track-showcount t
      erc-track-switch-direction 'leastactive
      erc-track-visibility 'visible
      erc-timestamp-only-if-changed-flag nil
      erc-timestamp-format "%D %T "
      erc-hide-timestamps t
      erc-pcomplete-order-nickname-completions t
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-header-line-format nil
      erc-auto-discard-away t
      erc-autoaway-idle-seconds (* 60 30)
      erc-autoaway-message "Away"
      erc-truncate-buffer-on-save t)

(add-to-list 'auto-coding-alist '("\\.erclogs/.*\\.txt" . utf-8))


;;--------------------
;;Colorize nick list
;;--------------------
;;colorize nick list. Source : emacs wiki
(setq nick-face-list '())
;; Define the list of colors to use when coloring IRC nicks.
(setq erc-colors-list '("green" "blue" "red"
			"dark gray" "brown" "dark orange"
			"dark magenta" "maroon"
			"indian red" "black" "forest green"
			"midnight blue" "dark violet"))

(defun build-nick-face-list ()
  "build-nick-face-list builds a list of new faces using the
foreground colors specified in erc-colors-list.  The nick faces
created here will be used to format IRC nicks."
  (setq i -1)
  (setq nick-face-list
	(mapcar
	 (lambda (COLOR)
	   (setq i (1+ i))
	   (list (custom-declare-face
		  (make-symbol (format "erc-nick-face-%d" i))
		  (list (list t (list :foreground COLOR)))
		  (format "Nick face %d" i))))
	 erc-colors-list)))

(defun my-insert-modify-hook ()
  "This insert-modify hook looks for nicks in new messages and
computes md5(nick) and uses substring(md5_value, 0, 4) mod (length
nick-face-list) to index the face list and produce the same face for a
given nick each time it is seen.  We get a lot of collisions this way,
unfortunately, but it's better than some other methods I tried.
Additionally, if you change the order or size of the erc-colors-list,
you'll change the colors used for nicks."
  (if (null nick-face-list) (build-nick-face-list))
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "<\\([^>]*\\)>")
	(let ((nick (match-string 1)))
	  (put-text-property (match-beginning 1) (match-end 1)
			     'face (nth
				    (mod (string-to-number
					  (substring (md5 nick) 0 4) 16)
					 (length nick-face-list))
				    nick-face-list))))))
(add-hook 'erc-insert-modify-hook 'my-insert-modify-hook)

;;--------------------
;;Unread messages bar
;;--------------------
(eval-after-load 'erc-track
  '(progn
     (defun erc-bar-move-back (n)
       "Moves back n message lines. Ignores wrapping, and server messages."
       (interactive "nHow many lines ? ")
       (re-search-backward "^.*<.*>" nil t n))

     (defun erc-bar-update-overlay ()
       "Update the overlay for current buffer, based on the content of
erc-modified-channels-alist. Should be executed on window change."
       (interactive)
       (let* ((info (assq (current-buffer) erc-modified-channels-alist))
	      (count (cadr info)))
	 (if (and info (> count erc-bar-threshold))
	     (save-excursion
	       (end-of-buffer)
	       (when (erc-bar-move-back count)
		 (let ((inhibit-field-text-motion t))
		   (move-overlay erc-bar-overlay
				 (line-beginning-position)
				 (line-end-position)
				 (current-buffer)))))
	   (delete-overlay erc-bar-overlay))))

     (defvar erc-bar-threshold 1
       "Display bar when there are more than erc-bar-threshold unread messages.")
     (defvar erc-bar-overlay nil
       "Overlay used to set bar")
     (setq erc-bar-overlay (make-overlay 0 0))
     (overlay-put erc-bar-overlay 'face '(:underline "black"))
     ;;put the hook before erc-modified-channels-update
     (defadvice erc-track-mode (after erc-bar-setup-hook
				      (&rest args) activate)
       ;;remove and add, so we know it's in the first place
       (remove-hook 'window-configuration-change-hook 'erc-bar-update-overlay)
       (add-hook 'window-configuration-change-hook 'erc-bar-update-overlay))
     (add-hook 'erc-send-completed-hook (lambda (str)
					  (erc-bar-update-overlay)))))

;;--------------------
;;channel change commands
;;--------------------
(defun irc-dwim (arg)
  "Runs IRC (by function irc, to be written for your particular servers)
  if it is not running, use erc-track to switch to last modified
  chan if it is."
  (interactive "p")
  (require 'erc-track)
  (if (erc-buffer-list)
      (my-track-switch-buffer arg)
    (irc)))
(global-set-key [f8] 'irc-dwim)

(defun my-track-switch-buffer (arg)
  "If there are unread messages, switch to them. Else, switch to latest seen non-erc buffer.
Differs a bit from erc's implementation : robust to buffer kills and stuff like that"
  (interactive "p")
  (if erc-modified-channels-alist
      (erc-track-switch-buffer arg)
    (let ((blist (buffer-list)))
      (while blist
	(unless (or (eq 'erc-mode (buffer-local-value 'major-mode (car blist)))
		    (minibufferp (car blist)))
	  (switch-to-buffer (car blist))
	  (setq blist nil))
	(setq blist (cdr blist))))))

;;--------------------
;; Tray control
;;--------------------
(defun erc-tray-update-state ()
  "Update the state of the tray icon. Blink when some new event
appears when you're not looking. Events are changes to
erc-modified-channels-alist, filtered by erc-tray-ignored-channels."
  (interactive)
  ;;stop blinking tray when there're no channels in list
  (unless erc-modified-channels-alist
    (erc-tray-change-state nil))
  ;;maybe make tray blink
  (unless (eq nil (frame-visible-p (selected-frame)))
    ;;filter list according to erc-tray-ignored-channels
    (let ((filtered-list erc-modified-channels-alist))
      (mapc (lambda (el)
	      (mapc (lambda (reg)
		      (when (string-match reg (buffer-name (car el)))
			(setq filtered-list
			      (remove el filtered-list))))
		    erc-tray-ignored-channels))
	    filtered-list)
      (when filtered-list
	(erc-tray-change-state t)))))

;;blink if away and activity
(add-hook 'erc-track-list-changed-hook 'erc-tray-update-state)

;;stop blinking whenever frame is set visible
(add-hook 'erc-mode-hook (lambda ()
			   (interactive)
			   (define-key special-event-map [make-frame-visible]
			     (lambda () (interactive)
			       (erc-bar-update-overlay)
			       (erc-tray-change-state nil)
			       (erc-modified-channels-update)))))


;;--------------------
;; Notification control
;;--------------------
(defun erc-notify-if-hl (matched-type nick msg)
  "Notify whenever someone highlights you and you're away"
  (when (and (eq matched-type 'current-nick)
	     (not (eq t (frame-visible-p (selected-frame)))))
    (notify (format "HL de %s" (erc-extract-nick nick))
	    msg)))
;;notify if away and highlighted
(add-hook 'erc-text-matched-hook 'erc-notify-if-hl)

(defun my-notify-JOIN (proc parsed)
  "Display notification of user connections on bitlbee"
  (let ((nick (erc-extract-nick (erc-response.sender parsed)))
	(chan (erc-response.contents parsed)))
    (when (string= chan "&bitlbee")
      (notify nick "Connexion MSN")))
  nil)
;;notify if someone joins on bitlbee
(add-hook 'erc-server-JOIN-functions 'my-notify-JOIN)

(defun my-notify-PRIVMSG (proc parsed)
  "Popup whenever someone privmsgs you and you're not seeing it"
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
	(target (car (erc-response.command-args parsed)))
	(msg (erc-response.contents parsed)))

    (when (and (string= target (erc-current-nick))
	       (not (eq t (frame-visible-p (selected-frame))))
	       (not (erc-is-message-ctcp-and-not-action-p msg)))
      ;;prevents from blinking on messages for which there is already
      ;;a notification
      ;; (setq erc-tray-inhibit-one-activation t)
      (notify (format "PM de %s" nick)
	      msg)))
  nil)
;;notify if away and pmed
(add-hook 'erc-server-PRIVMSG-functions 'my-notify-PRIVMSG)

;;--------------------
;;notify in query buffers when someone appears/disappears
;;--------------------
(erc-define-catalog
 'english
 '((my_notify_join      . "%n is back")
   (my_notify_quit      . "%n is gone")))

(defun my-notify-in-privmsg-JOIN (proc parsed)
  (let* ((nick (erc-extract-nick (erc-response.sender parsed)))
	 (buff (erc-get-buffer nick proc)))
    (when buff
      (erc-display-message
       parsed 'notice buff
       'my_notify_join ?n nick)))
  nil)
(add-hook 'erc-server-JOIN-functions 'my-notify-in-privmsg-JOIN)

(defun my-notify-in-privmsg-QUIT (proc parsed)
  (let* ((nick (erc-extract-nick (erc-response.sender parsed)))
	 (buff (erc-get-buffer nick proc)))
    (when buff
      (erc-display-message
       parsed 'notice buff
       'my_notify_quit ?n nick)))
  nil)
(add-hook 'erc-server-QUIT-functions 'my-notify-in-privmsg-QUIT)

;;--------------------
;;prompts for commands
;;--------------------
(defun erc-query-prompt ()
  "Prompts for someone to query"
  (interactive)
  (let ((completion-ignore-case t))
    (let ((server (erc-server-buffer))
	  (target (completing-read "Query sur: "
				   (erc-get-server-nickname-alist)
				   nil ;;no predicate, require match
				   t)))
      (erc-query target server))))
(defun erc-whois-prompt ()
  "Prompt for someone to do whois on"
  (interactive)
  (let ((completion-ignore-case t))
    (let ((target (completing-read "Whois sur: "
				   (erc-get-server-nickname-alist)
				   nil ;;no predicate, require match
				   t)))
      (erc-cmd-WHOIS target))))

(defun erc-names-prompt ()
  "Get names of channel, either using /names or blist if using bitlbee"
  (interactive)
  (if (string-match "&bitlbee" (buffer-name))
      (erc-send-message "root: blist")
    (erc-channel-names)))

;;--------------------
;; Setting away
;;--------------------
(require 'erc-autoaway)
(defun erc-toggle-away ()
  "Toggles away status in ERC."
  (interactive)
  (if (erc-away-time)
      (erc-autoaway-set-back)
    (erc-autoaway-set-away erc-autoaway-idle-seconds)))

;;--------------------
;; Toggle tracking
;;--------------------
(defvar erc-track-exclude '())
(defun toggle-channel-track ()
  "Toggle exclude status of current channel"
  (interactive)
  (let ((name (buffer-name (current-buffer))))
    (if (member name
		erc-track-exclude)
	(progn
	  (setq erc-track-exclude (remove name erc-track-exclude))
	  (message "Tracking on"))
      (progn
	(add-to-list 'erc-track-exclude name)
	(message "Tracking off")))))

;;--------------------
;; browse url before point with just a keystroke
;;--------------------
(require 'thingatpt)
(defun browse-url-before-point ()
  (interactive)
  (save-excursion
    (save-match-data
      (if (re-search-backward thing-at-point-url-regexp 0 t)
	  (browse-url (match-string 0))
	(message "Pas d'URL dans le buffer")))))

;;--------------------
;; view logs
;;--------------------
(defun erc-browse-log ()
  (interactive)
  (find-file (erc-current-logfile))
  (end-of-buffer))

;;--------------------
;; Setup keys
;;--------------------
(defun erc-setup-my-commands ()
  (interactive)
  (global-set-key [escape] 'irc-dwim)
  (global-set-key (kbd "<menu>") 'irc-dwim)
  (local-set-key (kbd "C-c C-a") 'erc-toggle-away)
  (local-set-key (kbd "C-c C-u") 'browse-url-before-point)
  (local-set-key (kbd "C-c C-q") 'erc-query-prompt)
  (local-set-key (kbd "C-c C-n") 'erc-names-prompt)
  (local-set-key (kbd "C-c C-w") 'erc-whois-prompt)
  (local-set-key (kbd "C-c C-l") 'erc-browse-log))
(add-hook 'erc-mode-hook 'erc-setup-my-commands)

;; I don't know why, something messes up with erc-bol, so I'm redefining it
(defun erc-bol ()
  "Move `point' to the beginning of the current line.

This places `point' just after the prompt, or at the beginning of the line."
  (interactive)
  ;;was (forward-line 0)
  (beginning-of-line)
  (when (get-text-property (point) 'erc-prompt)
    (goto-char erc-input-marker))
  (point))

(defun should-ignore (s)
  (when (string-match "^<.*> m+h$" s)
    (setq erc-insert-this nil)))
(add-hook 'erc-insert-pre-hook 'should-ignore)
