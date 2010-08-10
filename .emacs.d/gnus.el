;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Mime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display html or not?
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))
(setq mm-discouraged-alternatives '())
;; Offer to save in ~
(setq mm-default-directory "~/")
;; Useful for webcomics and such
(setq mm-inline-text-html-with-images t)
(setq mm-attachment-override-types '("image/.*"))

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
(define-key gnus-summary-mode-map (kbd "m")
  "\C-xo\276\C-rlink\C-mm\274\C-xo")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Misc
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
;; default : (setq gnus-group-line-format "%M%S%p%P%5y:%B%(%g%)%O\n")
;;(setq gnus-group-line-format "%y %(%G %)%O\n")
;;(setq gnus-summary-line-format "%U%R%z%(%[%d: %-20,20n%]%)%B %s\n")
;;(setq gnus-summary-line-format "%&user-date; %-30,30n%B%s\n")
;; the %uB invokes a function which returns the author name from BBDB
(setq gnus-summary-line-format "%&user-date; %-30,30uB %B%s\n")
(setq gnus-user-date-format-alist '((t . "%d.%m.%Y %H:%M")))

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
  (if gnus-group-display-unread
      (progn
	(gnus-group-list-groups gnus-level-subscribed nil)
	(setq gnus-group-display-unread nil))
    (gnus-group-list-all-groups gnus-level-subscribed)
    (setq gnus-group-display-unread t)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Receive mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; receive using dovecot as imap
(setq gnus-select-method '(nnimap "Mail" (nnimap-stream shell)))
(setq imap-shell-program "/usr/sbin/dovecot --exec-mail imap")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Send mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add Cc and Bcc headers to the message buffer
(setq message-default-mail-headers "Cc: \nBcc: \n")
;; postponed message is put in the following draft file
(setq message-auto-save-directory "~/Mail/drafts")
;; Use smtp for outgoing mail. Needs starttls. Under ubuntu, package starttls
(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)
;; Please be a bit more verbose if the connection fails.
(setq smtpmail-debug-info t)
;; Configuration of smtp in priv_gnus, see emacswiki, for instance http://www.emacswiki.org/emacs/GnusGmail#toc2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Notify. From Matthieu Moy, with modifs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun gnus-unread-demon-handler (&optional n)
  "Checks new mail under priority n, and notify authorities"
  (interactive)
  (gnus-demon-scan-news (if n n gnus-unread-level))
  (gnus-unread-update-unread-count))

(defun gnus-unread-update-unread-count ()
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
(add-hook 'gnus-exit-group-hook 'gnus-unread-update-unread-count t)
(add-hook 'gnus-select-article-hook 'gnus-unread-update-unread-count t)

;; every 1 gnus-demon-timestep, do a check
(defun gnus-unread-full-check ()
  (gnus-unread-demon-handler 7))
(gnus-demon-add-handler 'gnus-unread-full-check 1 nil)
;; check every 10 mins. Furthermore, a sync is done whenever offlineimap does a sync, with
;; postsynchook = emacsclient -e '(gnus-unread-demon-handler)'
(setq gnus-demon-timestep 600)
(gnus-demon-init)
;; if gnus doesn't respond in 10s, give up
(defadvice gnus-demon-scan-news (around gnus-demon-timeout activate)
  "Timeout for Gnus."
  (with-timeout
      (10 (message "Gnus timed out."))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BBDB
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
;; add recipients of mails to the bbdb, thanks to matthieu moy
(autoload 'bbdb/send-hook "moy-bbdb"
  "Function to be added to `message-send-hook' to notice records when sending messages" t)
(add-hook 'message-send-hook 'bbdb/send-hook)
;; add to bbdb if I'm recipient or cc'ed
(setq bbdb-ignore-most-messages-alist '(("to" . "levitt")
					("cc" . "levitt")))

(setq bbdb/mail-auto-create-p 'bbdb-ignore-most-messages-hook
      bbdb/news-auto-create-p 'bbdb-ignore-most-messages-hook)
(setq bbdb-use-pop-up nil
      bbdb-complete-name-allow-cycling t)
(setq bbdb-dwim-net-address-allow-redundancy t)


(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)

;; suggested in the man. Not sure that's useful though.
(setq gc-cons-threshold 3500000)
(setq gnus-newsgroup-maximum-articles nil)

(setq message-tab-body-function (lambda () (interactive) (dabbrev-expand nil)))


;; Personal info for password privacy
(load "~/.emacs.d/priv_gnus.el" t)
