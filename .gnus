;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Me
(setq user-mail-address "antoine.levitt@gmail.com")
(setq user-full-name "Antoine Levitt")
(setq gnus-ignored-from-addresses "")
; Don't bother me
(setq gnus-always-read-dribble-file t)
;; I don't use any other newsreader
(setq gnus-save-newsrc-file nil 
      gnus-read-newsrc-file nil)
(setq-default gnus-asynchronous t)
; default value was : "^to\\.\\|^[0-9. 	]+\\( \\|$\\)\\|^[\"][]\"[#'()]"
; must have been useful for some reason, around 1970.
(setq gnus-ignored-newsgroups "")
; stfu, kthx
(setq gnus-verbose 4)
;default : (setq gnus-group-line-format "%M%S%p%P%5y:%B%(%g%)%O\n")
(setq gnus-group-line-format "^%L ?%I !%T %M%S%p%m%P%5y: %(%G %)%O\n")

(setq gnus-summary-line-format "%U%R%z%(%[%4L: %-20,20f%]%)%B %s\n"
      gnus-summary-same-subject "")

(setq gnus-sum-thread-tree-root " >"
      gnus-sum-thread-tree-single-indent "  "
      gnus-sum-thread-tree-vertical "|"
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "+-> "
      gnus-sum-thread-tree-single-leaf "`-> ")


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
; Configuration in ~/.gnus_perso.el for password privacy
(load-file "~/.gnus_perso.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Notify. From Matthieu Moy, with modifs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; set for a specific notification level
(setq gnus-unread-level 3)
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
; check every 20 seconds
(setq gnus-demon-timestep 20)
(gnus-demon-init)
; if gnus doesn't respond in 10s, give up
(defadvice gnus-demon-scan-news (around gnus-demon-timeout activate)
  "Timeout for Gnus."
  (with-timeout
      (10 (message "Gnus timed out."))
    ad-do-it))

