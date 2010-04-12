(setq gnus-select-method '(nnimap "Mail" (nnimap-stream shell)))
(setq imap-shell-program "/usr/sbin/dovecot --exec-mail imap")

(setq user-mail-address "antoine.levitt@gmail.com")

(setq gnus-always-read-dribble-file t)
;; I don't use any other newsreader.
(setq
 gnus-save-newsrc-file nil 
 gnus-read-newsrc-file nil 
 )

; Use smtp for outgoing mail. Needs starttls. Under ubuntu, package starttls
(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)
; configuration, straight from emacswiki. Will be overwritten by ~/.gnus_perso.el
(setq message-send-mail-function 'smtpmail-send-it
      send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "username@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "yourcompany.com")

(load-file "~/.gnus_perso.el")
