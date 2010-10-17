;; require stuff. Needed because I define-keys on keymaps that need to be loaded
(require 'gnus-group)
(require 'gnus-sum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mairix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'nnmairix)
(define-key gnus-group-mode-map (kbd "s")
  'nnmairix-search)
(defadvice nnmairix-request-group (around nnmairix-be-quiet activate)
  "Be quiet. (avoids 'Matched n messages' when updating list)"
  (flet ((message (&rest args) ))
    ad-do-it))

;; setup with http://www.randomsample.de/nnmairix-doc/nnmairix.html. My .mairixrc is
;; base=~/.Maildovecot
;; maildir=.:.*
;; mformat=maildir
;; omit=.zz_mairix-*
;; database=~/.mairixdatabase
;; 
;; and I've got
;; @hourly mairix
;; in crontab -e

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display html or not?
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))
(setq mm-discouraged-alternatives '())
;; Offer to save in ~
(setq mm-default-directory "~/")
;; Useful for webcomics and such
(setq mm-inline-text-html-with-images t)
(setq mm-attachment-override-types '("image/.*"))
(setq gnus-blocked-images nil)

;; Use KH to view in a real browser
(defun my-gnus-summary-view-html-alternative ()
  "Display the HTML part of the current multipart/alternative MIME message
    in current browser."
  (interactive)
  (save-current-buffer
    (gnus-summary-show-article)
    (set-buffer gnus-article-buffer)
    (let ((file (make-temp-file "html-message-" nil ".html"))
	  (handle (cdr (assq 1 gnus-article-mime-handle-alist))))
      (mm-save-part-to-file handle file)
      (browse-url (concat "file://" file)))))
(define-key gnus-summary-mode-map (kbd "K H")
  'my-gnus-summary-view-html-alternative)
;; In summary, press m to go to the article in gwene
;; for w3m (setq mm-text-html-renderer 'w3m)
(define-key gnus-summary-mode-map (kbd "m")
  "\C-xo\276\C-rlink\C-mm\274\C-xo")
;; for gnus-html (setq mm-text-html-renderer 'gnus-article-html)
(define-key gnus-summary-mode-map (kbd "m")
  "\C-xo\276\C-rlink\C-m\C-m\274\C-xo")
;; add a separation between headers and body
(setq gnus-treat-body-boundary 'head)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I don't use any other newsreader
(setq gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil)
;; Please prefetch stuff
(setq gnus-asynchronous t)
;; default value was : "^to\\.\\|^[0-9. 	]+\\( \\|$\\)\\|^[\"][]\"[#'()]"
;; must have been useful for some reason, around 1970.
(setq gnus-ignored-newsgroups "")
;; stfu, kthx
(setq gnus-verbose 4)
(setq gnus-verbose-backends 4)
;; format dates in an user-friendly way
;; (setq gnus-user-date-format-alist '((t . "%d.%m.%Y %H:%M")))
(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "Today, %H:%M")
	((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
	(604800 . "%A %H:%M") ;;that's one week
	((gnus-seconds-month) . "%A %d")
	((gnus-seconds-year) . "%B %d")
	(t . "%B %d '%y"))) ;;this one is used when no other does match
;; default : (setq gnus-group-line-format "%M%S%p%P%5y:%B%(%g%)%O\n")
(setq gnus-group-line-format "%y %(%G %)%O\n")
;;(setq gnus-summary-line-format "%U%R%z%(%[%d: %-20,20n%]%)%B %s\n")
;;(setq gnus-summary-line-format "%&user-date; %-30,30n%B%s\n")
;; the %uB invokes a function which returns the author name from BBDB
;;(setq gnus-summary-line-format "%U%R%&user-date; %-30,30uB %*%B%s\n")
(setq gnus-summary-line-format "%U%R%~(max-right 17)~(pad-right 17)&user-date; %-30,30uB %*%B%s\n")

;; threading display
(setq gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-single-indent ""
      gnus-sum-thread-tree-vertical "|"
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "+-> "
      gnus-sum-thread-tree-single-leaf "`-> ")
(setq gnus-sum-thread-tree-vertical "│")
(setq gnus-sum-thread-tree-leaf-with-other "├─► ")
(setq gnus-sum-thread-tree-single-leaf "╰─► ")

;; smaller window for summary
(gnus-add-configuration
 '(article
   (vertical 1.0
	     (summary 0.2 point)
	     (article 1.0))))

(setq gnus-inhibit-startup-message t
      gnus-interactive-exit nil
      gnus-use-dribble-file nil
      ;; don't bother querying the server about unsubscribed groups
      gnus-activate-level gnus-level-subscribed)

;; toggle between read and unread articles. this is a bit of a hack, and should be better integrated.
;; oh well.
(setq gnus-group-display-unread nil)
(defun gnus-group-toggle-unread ()
  (interactive)
  (toggle-variable 'gnus-group-display-unread)
  (gnus-group-redisplay))
(defun gnus-group-redisplay ()
  "Redisplay group according to gnus-group-display-unread"
  (interactive)
  (if (not gnus-group-display-unread)
      (gnus-group-list-groups gnus-level-subscribed nil)
    (gnus-group-list-all-groups gnus-level-subscribed))
  (goto-char (point-min)))
(define-key gnus-group-mode-map (kbd "h") 'gnus-group-toggle-unread)



;;(setq gnus-thread-sort-functions '(gnus-thread-sort-by-number (not gnus-thread-sort-by-most-recent-number)))
(setq gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
(setq gnus-article-sort-functions '((not gnus-article-sort-by-date)))
(setq gnus-group-sort-function '(gnus-group-sort-by-alphabet gnus-group-sort-by-level))

(setq gnus-build-sparse-threads 'more)
;;(setq gnus-thread-hide-subtree t)
;; Gnus agent is buggy for imap, so don't use this for imap (J r in server buffer)
;; It's a known issue, but nobody seems to care.
(setq gnus-agent t)
;; compile stuff. No idea what it does, but it's supposed to be faster
(gnus-compile)

(setq gnus-large-newsgroup 1000)

(define-key gnus-group-mode-map (kbd "M-&") nil)
(define-key gnus-summary-mode-map (kbd "M-&") nil)
(define-key gnus-article-mode-map (kbd "M-&") nil)

(defun gnus-group-bury ()
  (interactive)
  (gnus-group-save-newsrc)
  (gnus-unread-update-unread-count)
  (condition-case err
      (kill-buffer "*Article*") ;;sometimes, articles stay after gnus is quitted.
					;I don't know why, I don't care, just kill them all!
    (error nil))
  (bury-buffer))
;; bury instead of gnus-group-exit.
(define-key gnus-group-mode-map (kbd "q") 'gnus-group-bury)

;; cleanup
(defadvice save-buffers-kill-emacs (before quit-gnus (&rest args) activate)
  (let (buf)
    (when (and (fboundp 'gnus-alive-p)
	       (gnus-alive-p)
	       (bufferp (setq buf (get-buffer "*Group*"))))
      (with-current-buffer buf
	(gnus-group-exit)))))

;; fill long
(require 'gnus-art)
(define-key gnus-summary-mode-map "l" 'gnus-article-fill-long-lines)

;; see what clients people use
(setq gnus-visible-headers 
      (concat gnus-visible-headers "\\|^User-Agent:\\|^X-Mailer:"))

;; if gnus doesn't respond in 15s, give up
(defadvice gnus-demon-scan-news (around gnus-demon-timeout activate)
  "Timeout for Gnus."
  (with-timeout
      (15 (message "Gnus timed out."))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Receive mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; receive using dovecot as imap
(setq gnus-select-method '(nnimap "Mail" (nnimap-stream shell)))
(setq imap-shell-program "/usr/sbin/dovecot --exec-mail imap")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Send mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Never split outgoing messages, most clients can't read them anyway.
;; It was probably very useful in the 70s.
(setq message-send-mail-partially-limit nil)
;; Use smtp for outgoing mail. Needs starttls. Under ubuntu, package starttls
(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)
;; Please be a bit more verbose if the connection fails.
(setq smtpmail-debug-info t)
;; Configuration of smtp in priv_gnus, see emacswiki, for instance http://www.emacswiki.org/emacs/GnusGmail#toc2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some of it is from Matthieu Moy, with modifs
(require 'gnus-group)
;; By default, gnus-group-get-new-news will force a redisplay of the group buffer
;; It's extremely annoying if you're doing something at the same time, and it messes
;; up with my "h" keybinding. Therefore, it must die.
(defadvice gnus-group-get-new-news (around gnus-group-get-new-news-dont-redisplay activate)
  "Don't redisplay at the end."
  (flet ((gnus-group-list-groups (&rest args) nil))
    ad-do-it))

;; But when I do a "g" on the buffer, I probably mean it.
(defun gnus-group-get-new-news-and-redisplay ()
  (interactive)
  (gnus-group-get-new-news)
  (gnus-group-redisplay))
(require 'gnus-group)
(define-key gnus-group-mode-map (kbd "g") 'gnus-group-get-new-news-and-redisplay)

(require 'gnus-demon)
;; set for a specific notification level
(setq gnus-unread-level 3) ;to override in personal settings
;; internal variables
(setq gnus-previous-unread-count 0)
(setq gnus-unread-count 0)
(defvar gnus-notify-modeline ""
  "Stuff that should be added to the modeline.")

(add-to-list 'global-mode-string
	     'gnus-notify-modeline
	     t)

(defun gnus-group-number-of-unread-mail (level)
  "*Returns the number of unread mails in groups of subscription level LEVEL and below."
  (with-current-buffer "*Group*"
    (let ((num-of-unread 0)
	  (newsrc (cdr gnus-newsrc-alist))
	  info clevel)
      (while newsrc
	(setq info (car newsrc)
	      clevel (gnus-info-level info))
	(when (<= clevel level)
	  (setq num-of-unread
		(+ num-of-unread (car (gnus-gethash (gnus-info-group info) gnus-newsrc-hashtb)))))
	(setq newsrc (cdr newsrc)))
      num-of-unread)))

;;REDEFINED from gnus-demon to allow levels
(defun gnus-demon-scan-news (&optional n)
  (let ((win (current-window-configuration)))
    (unwind-protect
	(save-window-excursion
	  (save-excursion
	    (when (gnus-alive-p)
	      (save-excursion
		(set-buffer gnus-group-buffer)
		(gnus-group-get-new-news n)))))
      (set-window-configuration win))))

(defun gnus-unread-check-news (&optional n)
  "Checks new mail under priority n (default gnus-unread-level), and notify authorities"
  (interactive)
  (gnus-demon-scan-news (if n n gnus-unread-level))
  (gnus-unread-update-unread-count))

(defun gnus-unread-update-unread-count ()
  "Update read count in the modeline"
  (interactive)
  (setq gnus-previous-unread-count gnus-unread-count)
  (setq gnus-unread-count (gnus-group-number-of-unread-mail gnus-unread-level))
  (setq gnus-notify-modeline
	( if (not (= 0 gnus-unread-count))
	    (format " Mail (%d)" gnus-unread-count)
	  ""))
  (if (and (> gnus-unread-count gnus-previous-unread-count)
	   (not (eq t (frame-visible-p (selected-frame)))))
      (notify "New mail !")))

(add-hook 'gnus-after-getting-new-news-hook 'gnus-unread-update-unread-count t)
(defun gnus-unread-refresh-and-update-unread-count ()
  "If displaying the Group buffer, refresh it. TODO fix for multiple windows"
  (interactive)
  (when (string= (buffer-name) "*Group*")
    (gnus-group-redisplay)
    (gnus-unread-update-unread-count)))
(add-hook 'window-configuration-change-hook 'gnus-unread-refresh-and-update-unread-count t)

;; full check once in a while. Furthermore, a sync for mail is done whenever offlineimap does a sync, with
;; postsynchook = emacsclient -e "(run-with-idle-timer 2 nil (lambda () (with-local-quit (gnus-unread-check-news))))"
;; the with-local-quit is because check-news is blocking, so we must provide a quit context
(defun gnus-unread-schedule-full-check ()
  (interactive)
  ;; next time the user is busy doing something else, ie when idle for 30s
  (run-with-idle-timer 30 nil (lambda () (with-local-quit (gnus-unread-check-news 5)))))
;; schedule full check every 10mins
(gnus-demon-add-handler 'gnus-unread-schedule-full-check 30 nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BBDB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'bbdb)
(require 'bbdb-hooks)
(bbdb-initialize 'gnus 'message)
(bbdb-insinuate-message)
(bbdb-insinuate-gnus)
(setq bbdb/gnus-summary-mark-known-posters nil)
(setq bbdb-always-add-addresses t)
(setq bbdb-new-nets-always-primary t)
(setq bbdb-offer-save 1) ; save without asking
;; add to bbdb only if I'm recipient or cc'ed. See below for bbdb-ignore-most-messages-alist
(setq bbdb-ignore-most-messages-alist nil)
(setq bbdb/mail-auto-create-p 'bbdb-ignore-most-messages-hook)
(setq bbdb-use-pop-up nil
      bbdb-complete-name-allow-cycling t)
(setq bbdb-dwim-net-address-allow-redundancy t)


(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)

;; suggested in the man. Not sure that's useful though.
(setq gc-cons-threshold 3500000)
(setq gnus-newsgroup-maximum-articles nil)

(setq message-tab-body-function (lambda () (interactive) (dabbrev-expand nil)))

(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq message-citation-line-format "%x %R, %N")

;; Personal info for password privacy
(load "~/.emacs.d/priv_gnus.el" t)

;; define this variable in priv_gnus
(when (boundp 'my-mail-addresses)
  ;; don't reply to my addresses
  (setq message-dont-reply-to-names my-mail-addresses)
  (dolist (el my-mail-addresses)
    ;; reply using the address the mail was sent to
    (add-to-list 'gnus-posting-styles `((header "to" ,el) (address ,el)))
    (add-to-list 'gnus-posting-styles `((header "cc" ,el) (address ,el)))
    (add-to-list 'gnus-posting-styles `((header "bcc" ,el) (address ,el)))
    ;; only add people who mail me directly to BBDB (no ML)
    (add-to-list 'bbdb-ignore-most-messages-alist `("to" . ,el))
    (add-to-list 'bbdb-ignore-most-messages-alist `("cc" . ,el))
    (add-to-list 'bbdb-ignore-most-messages-alist `("bcc" . ,el))))

;; ignore gwene (RSS to news gateway) messages. BBDB doesn't usually
;; notice news messages, but it does if the sender name is someone it
;; knows from mails.  Therefore, if I have a blog from someone I know
;; in my RSS feeds, bbdb will detect a dummy gwene address for that
;; person. This piece of code forbids it.
(setq bbdb-ignore-some-messages-alist '(("From" . "gwene")))
(setq bbdb-always-add-addresses 'bbdb-ignore-some-messages-hook)

;; When I reply to a message, I want gnus to exit the corresponding
;; summary buffer. 

;; advice that's used to exit the summary buffer
(setq gnus-advice-exit-summary
      '(let ((sumbuf (current-buffer)))
	 ad-do-it
	 (let ((replybuf (current-buffer)))
	   (with-current-buffer sumbuf
	     (gnus-summary-exit))
	   (switch-to-buffer replybuf))))

(defmacro gnus-add-exit-summary-to-function (fun)
  "Advices FUN so that it exits the summary buffer."
  (let ((advice-name
	 (make-symbol (concat (symbol-name (eval fun)) "-and-exit-summary"))))
    ;; the doc page for ad-add-advice says it won't add the same advice twice,
    ;; but it lies.
    (unless (ad-get-advice-info-field (eval fun) 'around)
      `(progn (ad-add-advice ,fun
			     '(,advice-name
			       nil
			       t
			       (lambda ()
				 ,gnus-advice-exit-summary))
			     'around
			     'last)
	      (ad-activate ,fun)))))

(gnus-add-exit-summary-to-function 'gnus-summary-reply)
(gnus-add-exit-summary-to-function 'gnus-summary-followup)
(gnus-add-exit-summary-to-function 'gnus-summary-mail-forward)
