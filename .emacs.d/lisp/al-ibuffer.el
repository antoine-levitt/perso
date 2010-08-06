;; Settings and code for ibuffer


;;never show emacs usual buffers or IRC servers
(setq ibuffer-never-show-predicates
      '("\\*scratch\\*"
	"\\*Messages\\*"
	"6667"))
;;separate buffers into groups
(setq ibuffer-saved-filter-groups
      '(("default"
	 ("ERC channel" (predicate (lambda ()
				     (and
				      (eq (current-mm)
					  'erc-mode)
				      (or
				       (string-match "^#"
						     (buffer-name (current-buffer)))
				       (string-match "^&"
						     (buffer-name (current-buffer))))))))
	 ("ERC pm" (predicate (lambda ()
				(eq (current-mm)
				    'erc-mode))))
	 ("Programmation" (or
			   (mode . c-mode)
			   (mode . c++-mode)
			   (mode . ada-mode)))
	 ("Text" (or
		  (mode . text-mode)
		  (mode . bibtex-mode)
		  (mode . latex-mode)))
	 ("Dired" (mode . dired-mode))
	 ("Diff" (mode . diff-mode))
	 ("Version control" (or
			     (name . "^\\*svn")
			     (name . "^\\*vc")))
	 ("Dotfiles" (name . "^\\."))
	 ("Help" (or
		  (name . "^\\*Completions\\*$")
		  (name . "^\\*Help\\*$")
		  (name . "^\\*Info\\*$")))
	 ("Special" (name . "^\\*")))))
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-show-empty-filter-groups nil)
;;make ibuffer window "popup"
(setq ibuffer-use-other-window t)
(setq ibuffer-default-shrink-to-minimum-size t)

;;visiting a file in ibuffer makes it "fullscreen"
(defadvice ibuffer-visit-buffer (after ibuffer-fs-after-visit (arg))
  "Delete other windows after visiting buffer"
  (delete-other-windows))
(ad-activate 'ibuffer-visit-buffer)

;;don't display header. source : emacs wiki.
(setq ibuffer-display-summary nil)
(setq ibuffer-use-header-line nil)
(defadvice ibuffer-update-title-and-summary (after kill-2-lines)
  (save-excursion
    (set-buffer "*Ibuffer*")
    (toggle-read-only 0)
    (goto-char 1)
    (search-forward "-\n" nil t)
    (delete-region 1 (point))
    (let ((window-min-height 1))
      ;; save a little screen estate
      (shrink-window-if-larger-than-buffer))
    (toggle-read-only)))
(ad-activate 'ibuffer-update-title-and-summary)

;;N when channel is ignored, else number of new messages
(define-ibuffer-column erc-modified (:name "M")
  (if (and (boundp 'erc-track-mode)
	   erc-track-mode)
      (if (member (buffer-name (current-buffer))
		  erc-track-exclude)
	  "N"
	(let ((entry (assq (current-buffer) erc-modified-channels-alist)))
	  (if entry
	      (propertize (int-to-string (cadr entry)) 'font-lock-face (cddr entry))
	    " ")))
    " "))

(setq ibuffer-formats
      '((mark erc-modified " " (name 18 18 :left :elide)
	      " " (size 9 -1 :right)
	      " " (mode 16 16 :left :elide) " " filename-and-process)
	(mark " " (name 16 -1) " " filename)))


(provide 'al-ibuffer)
