;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))
;(setq mm-discouraged-alternatives '())

; Don't bother me
(setq gnus-always-read-dribble-file t)
;; I don't use any other newsreader
(setq gnus-save-newsrc-file nil 
      gnus-read-newsrc-file nil)
(setq gnus-asynchronous t)
; default value was : "^to\\.\\|^[0-9. 	]+\\( \\|$\\)\\|^[\"][]\"[#'()]"
; must have been useful for some reason, around 1970.
(setq gnus-ignored-newsgroups "")
; stfu, kthx
(setq gnus-verbose 4)
;default : (setq gnus-group-line-format "%M%S%p%P%5y:%B%(%g%)%O\n")
(setq gnus-group-line-format "%y %(%G %)%O\n")

;(setq gnus-summary-line-format "%U%R%z%(%[%d: %-20,20n%]%)%B %s\n")
;(setq gnus-summary-line-format "%&user-date; %-30,30n%B%s\n")
;; the %uB invokes a function which returns the author name from BBDB
(setq gnus-summary-line-format "%&user-date; %-30,30uB %B%s\n")
(setq gnus-user-date-format-alist '((t . "%d.%m.%Y %H:%M")))


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
      ; don't bother querying the server about unsubscribed groups
      gnus-activate-level gnus-level-subscribed)

; toggle between read and unread articles. this is a bit of a hack, and should be better integrated.
; oh well.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Receive mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; receive using dovecot as imap
(setq gnus-select-method '(nnimap "Mail" (nnimap-stream shell)))
(setq imap-shell-program "/usr/sbin/dovecot --exec-mail imap")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Send mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add Cc and Bcc headers to the message buffer
(setq message-default-mail-headers "Cc: \nBcc: \n")
;; postponed message is put in the following draft file
(setq message-auto-save-directory "~/Mail/drafts")
; Use smtp for outgoing mail. Needs starttls. Under ubuntu, package starttls
(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)
; Please be a bit more verbose if the connection fails.
(setq smtpmail-debug-info t)
; Configuration, see emacswiki, for instance http://www.emacswiki.org/emacs/GnusGmail#toc2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Notify. From Matthieu Moy, with modifs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; set for a specific notification level
(setq gnus-unread-level 3) ;to override in personal settings
; will be updated
(setq gnus-previous-unread-count 0)
(setq gnus-unread-count 0)

(defvar gnus-notify-modeline ""
  "Stuff that should be added to the modeline.")

(add-to-list 'global-mode-string
	     'gnus-notify-modeline
	     t)

(defun gnus-group-number-of-unread-mail (level)
  "*Returns the number of unread mails in groups of subscription level LEVEL and below."
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
    num-of-unread))

(defun gnus-unread-demon-handler ()
  "Checks new mail, and notify authorities"
  (interactive)
  (gnus-demon-scan-news)
  (gnus-unread-update-unread-count))

(defun gnus-unread-update-unread-count ()
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

; every 1 gnus-demon-timestep, do a check
(gnus-demon-add-handler 'gnus-unread-demon-handler 1 nil)
; check every 2 mins. Furthermore, a sync is done whenever offlineimap does a sync, with
; postsynchook = emacsclient -e '(gnus-unread-demon-handler)'
(setq gnus-demon-timestep 120)
(gnus-demon-init)
; if gnus doesn't respond in 10s, give up
(defadvice gnus-demon-scan-news (around gnus-demon-timeout activate)
  "Timeout for Gnus."
  (with-timeout
      (10 (message "Gnus timed out."))
    ad-do-it))

;(setq gnus-thread-sort-functions '(gnus-thread-sort-by-number (not gnus-thread-sort-by-most-recent-number)))
(setq gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
(setq gnus-article-sort-functions '((not gnus-article-sort-by-date)))
(setq gnus-group-sort-function '(gnus-group-sort-by-alphabet gnus-group-sort-by-level))

(setq gnus-build-sparse-threads 'more)
;(setq gnus-thread-hide-subtree t)
;; Gnus agent is buggy for imap. It's a known issue, but nobody seems
;; to care. It's not really faster anyway, so disable
(setq gnus-agent nil)

(setq gnus-large-newsgroup 1000)

(define-key gnus-group-mode-map (kbd "M-&") nil)
(define-key gnus-summary-mode-map (kbd "M-&") nil)
;(define-key gnus-summary-buffer-map (kbd "M-&") nil)
(define-key gnus-article-mode-map (kbd "M-&") nil)

(defun gnus-group-bury ()
  (interactive)
  (gnus-group-save-newsrc)
  (gnus-unread-update-unread-count)
  (bury-buffer))
; bury instead of gnus-group-exit.
(define-key gnus-group-mode-map (kbd "q") 'gnus-group-bury)

(gnus-compile)

(defadvice save-buffers-kill-emacs (before quit-gnus (&rest args) activate)
  (let (buf)
    (when (and (fboundp 'gnus-alive-p)
	       (gnus-alive-p)
	       (bufferp (setq buf (get-buffer "*Group*"))))
      (with-current-buffer buf
	(gnus-group-exit)))))


;; BBDB
(require 'bbdb)
(require 'bbdb-hooks)
(bbdb-initialize 'gnus 'message)
(bbdb-insinuate-message)
(bbdb-insinuate-gnus)
(setq bbdb/gnus-summary-mark-known-posters nil)
(setq bbdb-always-add-addresses t)
(setq bbdb-new-nets-always-primary t)
(setq bbdb-offer-save 1) ; save without asking
; add recipients of mails to the bbdb, thanks to matthieu moy
(autoload 'bbdb/send-hook "moy-bbdb" 
  "Function to be added to `message-send-hook' to notice records when sending messages" t)
(add-hook 'message-send-hook 'bbdb/send-hook)
; add to bbdb if I'm recipient or cc'ed
(setq bbdb-ignore-most-messages-alist '(("to" . "levitt")
					("cc" . "levitt")))

(setq bbdb/mail-auto-create-p 'bbdb-ignore-most-messages-hook
      bbdb/news-auto-create-p 'bbdb-ignore-most-messages-hook)
(setq bbdb-use-pop-up nil
      bbdb-complete-name-allow-cycling t)
(setq bbdb-dwim-net-address-allow-redundancy t)


(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)

(setq mm-attachment-override-types '("image/.*"))

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

; suggested in the man. Not sure that's useful though.
(setq gc-cons-threshold 3500000)
(setq gnus-newsgroup-maximum-articles nil)

(setq message-tab-body-function (lambda () (interactive) (dabbrev-expand nil)))


; Personal info in ~/.gnus_perso.el for password privacy
(load "~/.gnus_perso.el" t)
