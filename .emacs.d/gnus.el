;; require stuff. Needed because I define-keys on keymaps that need to be loaded
(require 'gnus-group)
(require 'gnus-sum)
(require 'gnus-art)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gnus-inhibit-startup-message t
      gnus-interactive-exit nil
      gnus-use-dribble-file nil
      ;; don't bother querying the server about unsubscribed groups
      gnus-activate-level gnus-level-unsubscribed
      gnus-activate-foreign-newsgroups gnus-level-unsubscribed
      ;; I don't use any other newsreader, so don't clutter ~
      gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil
      ;; Please prefetch stuff
      gnus-asynchronous t
      ;; Don't ignore newsgroups. Default ignores []
      gnus-ignored-newsgroups ""
      ;; Don't gcc
      gnus-message-archive-group nil
      ;; Be quiet
      gnus-verbose 4
      gnus-verbose-backends 4
      ;; Sort things
      gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
      gnus-article-sort-functions '((not gnus-article-sort-by-date))
      gnus-group-sort-function '(gnus-group-sort-by-alphabet gnus-group-sort-by-level)
      ;; Don't display groups without unread messages
      gnus-list-groups-with-ticked-articles nil
      ;; Try to fill gaps in threads
      gnus-build-sparse-threads 'some
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
      ;; Threads display
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-single-indent ""
      gnus-sum-thread-tree-vertical "|"
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "+-> "
      gnus-sum-thread-tree-single-leaf "`-> "
      gnus-sum-thread-tree-vertical "│"
      gnus-sum-thread-tree-leaf-with-other "├─► "
      gnus-sum-thread-tree-single-leaf "╰─► "
      ;; See what client people use
      gnus-visible-headers (concat gnus-visible-headers "\\|^User-Agent:\\|^X-Mailer:")
      ;; Dates for humans
      gnus-user-date-format-alist
      '(((gnus-seconds-today) . "Today, %H:%M")
	((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
	(604800 . "%A %H:%M")
	((gnus-seconds-month) . "%A %d")
	((gnus-seconds-year) . "%B %d")
	(t . "%B %d '%y"))
      ;; Cleaner format for groups and summaries
      gnus-group-line-format "%-3,3y%(%G %)\n"
      ;; the %uB invokes a function which returns the author name from BBDB
      gnus-summary-line-format "%U%R%~(max-right 17)~(pad-right 17)&user-date; %-30,30uB %*%B%s\n"
      gnus-summary-mode-line-format "Gnus: %g %Z"
      gnus-article-mode-line-format "Gnus: %S"
      ;; simpler group mode line
      gnus-group-mode-line-format "Gnus"
      ;; make n and p ignore unread status
      gnus-group-goto-unread nil
      
      ;; Performance-related settings
      ;; How large is large?
      gnus-large-newsgroup 200
      ;; don't silently hide messages
      gnus-newsgroup-maximum-articles nil
      ;; Never use agent, since all my groups are local
      gnus-agent nil

      ;; Message
      ;; Complete using dabbrev
      message-tab-body-function (lambda () (interactive) (dabbrev-expand nil))
      message-citation-line-function 'message-insert-formatted-citation-line
      ;; 01/01/1970, XX
      message-citation-line-format "%x %R, %N"
      ;; don't bug me with cancel locks
      message-insert-canlock nil
      message-generate-new-buffers 'unique
      message-make-forward-subject-function 'message-forward-subject-fwd
      message-wash-forwarded-subjects t)

;; compile stuff. No idea what it does, but it's supposed to be faster
(gnus-compile)

;; Unbind stuff I use for something else
(define-key gnus-group-mode-map (kbd "M-&") nil)
(define-key gnus-summary-mode-map (kbd "M-&") nil)
(define-key gnus-article-mode-map (kbd "M-&") nil)

;; smaller window for summary
(gnus-add-configuration
 '(article
   (vertical 1.0
	     (summary 0.2 point)
	     (article 1.0))))

;; toggle between read, unread and subscribed articles. this is a bit of a hack, and should be better integrated.
;; oh well.
(defvar gnus-group-display-state 'unread
  "What to display in the group buffer.")
(defun gnus-group-redisplay ()
  "Redisplay group according to gnus-group-display-unread"
  (interactive)
  (case gnus-group-display-state
    ('unread (gnus-group-list-groups gnus-level-subscribed nil))
    ('read (gnus-group-list-groups gnus-level-unsubscribed t))
    ('unsubscribed (gnus-group-list-groups gnus-level-unsubscribed nil)))
  (goto-char (point-min))
  )
(defun gnus-group-toggle-read ()
  (interactive)
  (setq gnus-group-display-state
	(if (equal gnus-group-display-state 'read)
	    'unread
	  'read))
  (gnus-group-redisplay))
(defun gnus-group-toggle-unsubscribed ()
  (interactive)
  (setq gnus-group-display-state
	(if (equal gnus-group-display-state 'unsubscribed)
	    'unread
	  'unsubscribed))
  (gnus-group-redisplay)
  (goto-char (point-min)))
(define-key gnus-group-mode-map (kbd "h") 'gnus-group-toggle-read)
(define-key gnus-group-mode-map (kbd "j") 'gnus-group-toggle-unsubscribed)

(define-key gnus-group-mode-map (kbd "A !") (lambda () (interactive) (gnus-group-list-ticked 10)))

(defun gnus-group-bury ()
  (interactive)
  (gnus-group-save-newsrc)
  (gnus-unread-update-unread-count)
  (condition-case err
      ;;sometimes, articles stay after gnus is quitted.
      ;;I don't know why, I don't care, just kill them all!
      (kill-buffer "*Article*")
    (error nil))
  (bury-buffer))
;; bury instead of gnus-group-exit.
(define-key gnus-group-mode-map (kbd "q") 'gnus-group-bury)
;; bind Q to exit cleanly
(define-key gnus-group-mode-map (kbd "Q") 'gnus-group-exit)

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

(defun gnus-summary-toggle-thread-hiding ()
  (interactive)
  (make-local-variable 'are-threads-hidden)
  (if (and (boundp 'are-threads-hidden) are-threads-hidden)
      (progn
	(gnus-summary-show-all-threads)
	(setq are-threads-hidden nil))
    (gnus-summary-hide-all-threads)
    (setq are-threads-hidden t)))
(define-key gnus-summary-mode-map (kbd "H") 'gnus-summary-toggle-thread-hiding)

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
(setq mm-discouraged-alternatives '("text/html" "text/richtext")
      mm-discouraged-alternatives '()
      ;; Offer to save in ~
      mm-default-directory "~/"
      ;; Render with gnus-w3m. Also available is shr
      mm-text-html-renderer 'shr
      ;; Display images
      mm-inline-text-html-with-images t
      mm-attachment-override-types '("image/.*")
      gnus-blocked-images "doubleclick\\.net\\|feedsportal\\|hits\\.guardian\\|rss\\.lemonde")

(define-key gnus-summary-mode-map (kbd "h")
  'gnus-article-browse-html-article)
(setq gnus-article-browse-delete-temp t)
;; In summary buffer, press m to go to the article in gwene
(define-key gnus-summary-mode-map (kbd "m")
  "\C-xo\276\C-rlink\C-m\C-m\274\C-xo")
(define-key gnus-summary-mode-map (kbd "o") 'gnus-summary-save-parts)
;; Attach files in dired
(require 'gnus-dired)
(define-key dired-mode-map (kbd "a") 'gnus-dired-attach)

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
;; Configuration of smtp in priv_gnus, see emacswiki, for instance http://www.emacswiki.org/emacs/GnusGmail#toc2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nntp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq nntp-open-connection-function 'nntp-open-plain-stream)

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
(defvar gnus-notify-level 3
  "Notify for unread articles at this level or under")
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

(defun gnus-unread-update-unread-count ()
  "Update read count in the modeline"
  (interactive)
  (setq gnus-previous-unread-count gnus-unread-count)
  (setq gnus-unread-count (gnus-group-number-of-unread-mail gnus-notify-level))
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

;; full check every once in a while (should not be necessary since things that update
;; gnus call the update via emacsclient)
(gnus-demon-add-handler 'gnus-group-get-new-news 30 nil)

;; if gnus doesn't respond in 5s, give up
(defadvice gnus-group-get-new-news (around gnus-demon-timeout activate)
  "Timeout for Gnus."
  (with-timeout
      (5 (message "Gnus timed out."))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal info for password privacy
(load "~/.emacs.d/priv_gnus.el" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BBDB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'bbdb)
(bbdb-initialize 'gnus 'message)
(bbdb-mua-auto-update-init 'gnus 'message)

(setq bbdb-update-records-p t ; don't prompt me for creation
      bbdb/gnus-update-records-p 'bbdb-select-message ; filter incoming mail and add those who mail me
      bbdb/message-update-records-p t ; automatically add everybody I send mail to
      bbdb/gnus-summary-mark-known-posters nil ; I don't care about this feature
      bbdb-mail-allow-redundancy t ; allow full-name completion
      bbdb-complete-mail-allow-cycling t ; allow cycling
      bbdb-new-mails-always-primary t ; always use newly added mails
      bbdb-pop-up-window-size 5 ; small window
      bbdb-completion-display-record nil
      bbdb-message-pop-up nil) ; do not popup me

(define-key bbdb-mode-map (kbd "q") 'quit-window)

;; define this variable in priv_gnus
(when (boundp 'my-mail-addresses)
  ;; don't reply to my addresses
  (setq message-dont-reply-to-names my-mail-addresses)
  ;; only add to bbdb people who mail me directly (no mailing lists)
  (setq bbdb-accept-message-alist `(("to" . ,(regexp-opt my-mail-addresses))
				    ("cc" . ,(regexp-opt my-mail-addresses))
				    ("bcc" . ,(regexp-opt my-mail-addresses))))
  (dolist (el my-mail-addresses)
    ;; reply using the address the mail was sent to
    (add-to-list 'gnus-posting-styles `((header "to" ,el) (address ,el)))
    (add-to-list 'gnus-posting-styles `((header "cc" ,el) (address ,el)))
    (add-to-list 'gnus-posting-styles `((header "bcc" ,el) (address ,el)))))
(add-hook 'bbdb-after-change-hook (lambda (arg)
				    (flet ((message (&rest args) nil))
				      (bbdb-save))))
