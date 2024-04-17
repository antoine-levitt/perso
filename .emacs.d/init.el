;;; Emacs of Antoine Levitt. Homepage : http://github.com/antoine-levitt/perso
;; Mainly a mix of many things I found on the net, plus some stuff of mine

(if (string-match "beta" (shell-command-to-string
			  "hostname"))
    (set-frame-font "Noto Mono 16"))
(if (string-match "lambda" (shell-command-to-string
			  "hostname"))
    (set-frame-font "Noto Mono 16"))
;; (set-frame-font "Noto Mono 22") ; good for projectors

;; customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
 '(horizontal-scroll-bar-mode nil)
 '(inhibit-startup-echo-area-message (user-login-name))
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(warning-suppress-log-types '((comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gnus-button ((t (:inherit nil)))))

;; Packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))


(setq myPackages
      '(material-theme
        auctex
        rainbow-delimiters
        julia-mode
        matlab-mode
        smartparens
        cython-mode
        undo-tree
        highlight-indentation
        magit
        magic-latex-buffer
        ivy
        counsel
        flx
        mu4e-alert
        pdf-tools
        iedit
        ;; smart-mode-line
        avy
        markdown-mode
	guess-language
	julia-repl
	eterm-256color
	electric-operator
	forge
	vterm
	fix-word
	ivy-prescient
	expand-region
	quelpa
	sqlite3
	visual-regexp
	visual-regexp-steroids
	spaceline
	))
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

(defun dummy-function (&rest args) nil)
(defun remove-mm-lighter (mm)
  "Remove minor lighter from the mode line."
  (setcar (cdr (assq mm minor-mode-alist)) nil))

;; Better better defaults
(defun isearch-backward-word (&optional not-word no-recursive-edit)
  "Do incremental search backward for a sequence of words.
With a prefix argument, do a regular string search instead.
Like ordinary incremental search except that your input is treated
as a sequence of words without regard to how the words are separated.
See the command `isearch-backward' for more information.
This command does not support character folding, and lax space matching
has no effect on it."
  (interactive "P\np")
  (isearch-mode nil nil nil (not no-recursive-edit) (null not-word)))

(global-set-key (kbd "C-M-s") 'isearch-forward-word)
(global-set-key (kbd "C-M-r") 'isearch-backward-word)
(setq sentence-end-double-space nil)
(setq confirm-kill-processes nil)

;; suggested by lsp
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unclutter home directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; put everything in ~/.emacs.d
(setq gnus-init-file "~/.emacs.d/gnus.el"
      gnus-home-directory "~/.emacs.d"
      mail-default-directory "~/.emacs.d"
      message-directory "~/.emacs.d/Mail"
      bbdb-file "~/.emacs.d/bbdb")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Desktop and server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;if we are alone, run server, and load desktop
;;very crude hack
(setq emacs-is-master nil)
(when (string= "1\n"
	       (shell-command-to-string
		"ps x | grep emacs | grep -v grep | grep -v emacs-bin | grep -v emacsclient | grep -v gpg-agent | wc -l"))
  (setq emacs-is-master t)
  (server-start)
  (setenv "EDITOR" "emacsclient")

  ;; desktop
  (setq desktop-load-locked-desktop t
	desktop-path (list (expand-file-name "~/.emacs.d/"))
	desktop-dirname (expand-file-name "~/.emacs.d/")
	desktop-base-file-name "emacs.desktop"
	desktop-files-not-to-save "\\(^/[^/:]*:\\|(ftp)$\\|synctex.gz\\)")
  ;; remove auto-fill
  (setq desktop-minor-mode-table '((auto-fill-function nil)
                                   (defining-kbd-macro nil)
                                   (isearch-mode nil)
                                   (vc-mode nil)
                                   (vc-dired-mode nil)
                                   (erc-track-minor-mode nil)
                                   (savehist-mode nil)))
  (desktop-save-mode 1)
  ;; save every 10mins
  (run-with-timer (* 10 60) (* 10 60) (lambda ()
                                        (cl-letf (((symbol-function 'message) 'dummy-function))
                                          (desktop-save-in-desktop-dir))))
  )
;; greeting message
(add-hook 'after-init-hook (lambda () (set-frame-parameter nil 'fullscreen 'fullboth)))
(add-hook 'after-init-hook (lambda () (message "Welcome back.")) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graphical display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; there is some stuff in customize, but can't move it
;; here for technical reasons

;; no right fringe
(fringe-mode '(nil . 0))
(setq ring-bell-function (lambda () nil)) ; I don't like bells

;; fullscreen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth (frame-parameter nil 'fullscreen))
			     nil
			   'fullboth))))
(defalias 'fs 'toggle-fullscreen)
;; one emacs to rule them all and in fullscreen bind them
(when emacs-is-master
  (set-frame-parameter nil 'fullscreen 'fullboth))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mouse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; control mouse clipboard. In particular, select-active-regions, activated in 23.2, sucks.
(setq select-active-regions nil)
(setq mouse-highlight 1)
;; yank at point instead of at click
(setq mouse-yank-at-point t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General-purpose functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun current-mm ()
  (buffer-local-value 'major-mode (current-buffer)))

(defun current-buffer-not-mini ()
  "Return current-buffer if current buffer is not the *mini-buffer*
  else return buffer before minibuf is activated."
  (if (not (window-minibuffer-p)) (current-buffer)
    (if (eq (get-lru-window) (next-window))
	(window-buffer (previous-window)) (window-buffer (next-window)))))

(defun launch-command (command filename)
  "Launches command with argument filename, discarding all output"
  (call-process command nil 0 nil filename)
  )

(defun gnome-open-file (filename)
  "gnome-opens the specified file."
  (interactive "fFile to open: ")
  (launch-command  "/usr/bin/xdg-open" filename))

(defun basename-cons(f)
  (cons (file-name-nondirectory f) f))

(defun sudo-edit (&optional arg)
  "Edit a file as root"
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun toggle-alternate-file-as-root (&optional filename)
  "Toggle between the current file as the default user and as root.
From http://atomized.org/2011/01/toggle-between-root-non-root-in-emacs-with-tramp/"
  (interactive)
  (let* ((filename (or filename (buffer-file-name)))
         (parsed (when (tramp-tramp-file-p filename)
                   (coerce (tramp-dissect-file-name filename)
                           'list)))
         (old-pnt (point)))
    (unless filename
      (error "No file in this buffer."))

    (unwind-protect
        (find-alternate-file
         (if (equal '("sudo" "root") (butlast parsed 2))
             ;; As non-root
             (if (or
                  (string= "localhost" (nth 2 parsed))
                  (string= (system-name) (nth 2 parsed)))
                 (nth 3 parsed)
               (apply 'tramp-make-tramp-file-name
                      (append (list tramp-default-method nil) (cddr parsed))))

           ;; As root
           (if parsed
               (apply 'tramp-make-tramp-file-name
                      (append '("sudo" "root") (cddr parsed)))
             (tramp-make-tramp-file-name "sudo" "root" "localhost" filename))))
      (goto-char old-pnt))))
(global-set-key (kbd "C-c C-r") 'toggle-alternate-file-as-root)

;; https://www.gnu.org/software/emacs/manual/html_node/eintr/the_002dthe.html
(defun the-the ()
  "Search forward for for a duplicated word."
  (interactive)
  (message "Searching for for duplicated words ...")
  (push-mark)
  ;; This regexp is not perfect
  ;; but is fairly good over all:
  (if (re-search-forward
       "\\b\\([^@ \n\t]+\\)[ \n\t]+\\1\\b" nil 'move)
      (message "Found duplicated word.")
    (message "End of buffer")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc. settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; I don't use bidi stuff
(setq-default bidi-display-reordering nil)
;; No fancy new messages that stay for a while
(setq minibuffer-message-timeout 0)
;; instead of / or whatever
(setq default-directory (expand-file-name "~/"))
;; OH MY GOD IT'S A SECURITY VULNERABILITY, WE ARE ALL GONNA DIE
(setq enable-local-variables :all)
;;automatic indent
(global-set-key (kbd "RET") 'newline-and-indent)
;;no transient mark
(transient-mark-mode -1)
;; fix behavior of quit-window
(defadvice quit-window (around back-to-one-window activate)
  "If there are exactly two windows open (typically, you're editing one file and
some other pops up with display-buffer), go back to only one window open"
  (if (= 2 (length (window-list)))
      (progn
	(let ((buffer (window-buffer window)))
	  (delete-other-windows (other-window 1))
	  (if kill
	      (kill-buffer buffer)
	    (bury-buffer buffer))))
    ad-do-it))

;; bypass emacs broken mechanism to detect browser
(setq browse-url-browser-function
      (lambda (url &rest args)
	(interactive)
	(launch-command "x-www-browser" url)))

;;just type y/n instead of yes/no RET. this should be default
(fset 'yes-or-no-p 'y-or-n-p)

;;save the minibuffer input
(savehist-mode 1)

;;save last edit place in files
(require 'saveplace)
(save-place-mode 1)
(setq save-place-file "~/.emacs.d/places")

;;blinking cursor is distracting and useless
(blink-cursor-mode -1)

;;don't display tooltips
(setq tooltip-delay 10000000)

;;display buffer name in title bar
(setq frame-title-format "%b - Emacs")
(setq icon-title-format "%b - Emacs")

;;backups/autosaves : no autosaves, and backups in one centralised place
(setq auto-save-default nil)
(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))

;;move between windows with shift-arrows
(windmove-default-keybindings 'shift)

;;please add a final newline each time I save a buffer
(setq require-final-newline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;scroll one line at a time
(setq scroll-conservatively 100000000)
;;keep cursor at current position when scrolling
(setq scroll-preserve-screen-position 'stay)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Silent saves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note that this can not prevent
;; the "Wrote %s" message, which is coded in C.
(defadvice save-buffer (around save-be-quiet activate)
  "Be quiet."
  (cl-letf (((symbol-function 'message) 'dummy-function))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Word wrapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; amazing new variable in e23. No need to worry about longlines any more
;; (setq-default word-wrap t)
(global-visual-line-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; display time in the bar, insert time in buffers
(setq display-time-default-load-average nil)
(setq display-time-24hr-format t)
(display-time-mode 1)
(defun insert-time ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %R")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto revert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;automatically update buffers when changed
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers nil)
(setq auto-revert-interval 5)
(setq auto-revert-verbose nil)
(add-hook 'dired-mode-hook 'auto-revert-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Undo-tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'undo-tree)
(setq undo-tree-mode-lighter "")
(setq undo-tree-auto-save-history nil)
(global-undo-tree-mode)
(add-hook 'fundamental-mode-hook 'turn-on-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parenthesis editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;visual paren matching
(setq show-paren-delay 0)
(show-paren-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc. dired add-ons
(require 'dired-x)
;; omit
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(add-to-list 'dired-omit-extensions ".log")
(add-to-list 'dired-omit-extensions ".out")
(add-to-list 'dired-omit-extensions ".synctex.gz")
;;clean dired default view : omit hidden files, don't display groups, use human-readable sizes
(setq dired-listing-switches "-alhGv"
      dired-free-space-args "-Pkm"
      dired-auto-revert-buffer t
      dired-recursive-copies 'always
      ;; dired-recursive-deletes 'always
      dired-clean-confirm-killing-deleted-buffers nil)
;; Omit, be quiet
(defadvice dired-omit-expunge (around dired-omit-be-quiet activate)
  "Be quiet."
  (cl-letf (((symbol-function 'message) 'dummy-function))
    ad-do-it))
(add-hook 'dired-mode-hook 'dired-omit-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(define-key dired-mode-map (kbd "o") 'dired-find-alternate-file)
(put 'dired-find-alternate-file 'disabled nil)
;;add xdg-open as C-ret
(defun dired-xdg-open-file ()
  "Opens the current file in a Dired buffer."
  (interactive)
  (launch-command "xdg-open" (dired-get-file-for-visit)))
(define-key dired-mode-map (kbd "<C-return>") 'dired-xdg-open-file)
(defun smplayer-open-file ()
  (interactive)
  (launch-command "smplayer" (dired-get-file-for-visit)))
(define-key dired-mode-map (kbd "²") 'smplayer-open-file)
(define-key dired-mode-map (kbd "œ") 'smplayer-open-file)
(define-key dired-mode-map (kbd "M-o") 'dired-omit-mode)
(define-key dired-mode-map (kbd "l") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-j") 'dired-find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ignore case when matching a suffix (such as .F90)
(setq auto-mode-case-fold t)
;;tags
(setq tags-table-list nil
      tags-revert-without-query t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Matlab
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'matlab)
(setq matlab-indent-function t)
(setq matlab-verify-on-save-flag nil)
(setq matlab-auto-fill nil)
(setq matlab-fill-code nil)
(setq matlab-shell-command-switches '("-nojvm"))
(define-key matlab-mode-map (kbd "M-q") nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cc-mode)
;;linux style
(setq c-default-style "linux")
;;'electric' indentation : indent on newline
(define-key c-mode-base-map "\C-m"
  'c-context-line-break)
(define-key c-mode-base-map (kbd "M-q") nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pdf-tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pdf-tools-install)
(setq jka-compr-verbose nil)

(define-key pdf-view-mode-map (kbd "m") 'pdf-view-midnight-minor-mode)
(define-key pdf-view-mode-map (kbd "s-a") 'bury-buffer)
(define-key pdf-view-mode-map (kbd "n") 'pdf-view-scroll-up-or-next-page)
(define-key pdf-view-mode-map (kbd "C-v") 'pdf-view-scroll-up-or-next-page)
(define-key pdf-view-mode-map (kbd "v") 'pdf-view-scroll-up-or-next-page)
(define-key pdf-view-mode-map (kbd "p") 'pdf-view-scroll-down-or-previous-page)
(define-key pdf-view-mode-map (kbd "M-v") 'pdf-view-scroll-down-or-previous-page)
(define-key pdf-view-mode-map (kbd "V") 'pdf-view-scroll-down-or-previous-page)
(define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
(define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
(define-key pdf-view-mode-map (kbd "p") 'pdf-view-previous-page-command)
(define-key pdf-view-mode-map (kbd "n") 'pdf-view-next-page-command)

(define-key pdf-view-mode-map (kbd "j") (lambda () (interactive) (pdf-view-scroll-up-or-next-page 5)))
(define-key pdf-view-mode-map (kbd "k") (lambda () (interactive) (pdf-view-scroll-down-or-previous-page 5)))
(define-key pdf-view-mode-map (kbd "q") 'bury-buffer)
(define-key pdf-view-mode-map (kbd "a") 'pdf-annot-add-text-annotation)
(define-key pdf-view-mode-map (kbd "c") (lambda () (interactive) (launch-command "xdg-open" (buffer-file-name))))

(setq-default pdf-view-display-size 'fit-height)
(setq TeX-view-program-selection '((output-pdf "pdf-tools")))
(setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))

(setq pdf-view-midnight-colors (cons (frame-parameter nil 'foreground-color) (frame-parameter nil 'background-color)))
(add-hook 'after-init-hook (lambda () (setq pdf-view-midnight-colors (cons (frame-parameter nil 'foreground-color) (frame-parameter nil 'background-color)))))


;; ;; No timeout for tooltip
;; ;; Temp fix until https://github.com/politza/pdf-tools/pull/650
;; (defun pdf-util-tooltip-arrow (image-top &optional timeout)
;;   (pdf-util-assert-pdf-window)
;;   (when (floatp image-top)
;;     (setq image-top
;;           (round (* image-top (cdr (pdf-view-image-size))))))
;;   (let* (x-gtk-use-system-tooltips ;allow for display property in tooltip
;;          (dx (+ (or (car (window-margins)) 0)
;;                 (car (window-fringes))))
;;          (dy image-top)
;;          (pos (list dx dy dx (+ dy (* 2 (frame-char-height)))))
;;          (vscroll
;;           (pdf-util-required-vscroll pos))
;;          (tooltip-frame-parameters
;;           `((border-width . 0)
;;             (internal-border-width . 0)
;;             ,@tooltip-frame-parameters))
;;          ;(tooltip-hide-delay (or timeout 6))
;;          )
;;     (when vscroll
;;       (image-set-window-vscroll (* vscroll
;; 				   (if pdf-view-have-image-mode-pixel-vscroll
;; 				       (frame-char-height) 1))))
;;     (setq dy (max 0 (- dy
;;                        (cdr (pdf-view-image-offset))
;;                        (window-vscroll nil t)
;;                        (frame-char-height))))
;;     (when (overlay-get (pdf-view-current-overlay) 'before-string)
;;       (let* ((e (window-inside-pixel-edges))
;;              (xw (pdf-util-with-edges (e) e-width)))
;;         (cl-incf dx (/ (- xw (car (pdf-view-image-size t))) 2))))
;;     (pdf-util-tooltip-in-window
;;      (propertize
;;       " " 'display (propertize
;; 		    "\u2192" ;;right arrow
;; 		    'display '(height 2)
;; 		    'face `(:foreground
;;                             "orange red"
;;                             :background
;;                             ,(if (bound-and-true-p pdf-view-midnight-minor-mode)
;;                                  (cdr pdf-view-midnight-colors)
;;                                "white"))))
;;      dx dy)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Latex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;don't ask to cache preamble
(setq preview-auto-cache-preamble t)
(setq preview-preserve-counters t)
(setq preview-scale-function 1.6)
;;use synctex for synchronisation with viewer
(setq TeX-source-correlate-method 'synctex)

(require 'magic-latex-buffer)

;; add my own symbols
(defconst ml/symbols
  (append (mapcar (lambda (pattern)
                    (cons (concat "\\\\not[ \t\n]*" (car pattern))
                          (compose-string (concat "／" (cdr pattern)))))
                  (append ml/relation-symbols ml/arrow-symbols))
          ml/decoration-commands
          ml/relation-symbols
          ml/negrel-symbols
          ml/operator-symbols
          ml/arrow-symbols
          ml/letter-symbols
          ml/other-symbols
          ml/accents
          '(("\\\\eps\\>" . "ε"))
          '(("\\\\R\\>" . "ℝ"))
          '(("\\\\Z\\>" . "ℤ"))
          '(("\\\\N\\>" . "ℕ"))
          '(("\\\\C\\>" . "ℂ"))
	  ))

(add-hook 'TeX-update-style-hook 'magic-latex-buffer)
(setq magic-latex-enable-block-align nil)
(setq magic-latex-enable-block-highlight nil)

(setq TeX-newline-function 'newline-and-indent
      LaTeX-math-abbrev-prefix (kbd "ù")
      TeX-electric-sub-and-superscript t
      TeX-save-query nil)

;; found on http://tex.stackexchange.com/questions/69555/i-want-to-disable-auto-fill-mode-when-editing-equations
(defvar my-LaTeX-no-autofill-environments
  '("equation" "equation*" "align" "align*")
  "A list of LaTeX environment names in which `auto-fill-mode' should be inhibited.")
(defun my-LaTeX-auto-fill-function ()
  "This function checks whether point is currently inside one of
the LaTeX environments listed in
`my-LaTeX-no-autofill-environments'. If so, it inhibits automatic
filling of the current paragraph."
  (let ((do-auto-fill t)
        (current-environment "")
        (level 0))
    (while (and do-auto-fill (not (string= current-environment "document")))
      (setq level (1+ level)
            current-environment (LaTeX-current-environment level)
            do-auto-fill (not (member current-environment my-LaTeX-no-autofill-environments))))
    (when do-auto-fill
      (do-auto-fill))))

(setq LaTeX-math-list '((?/ "frac" nil nil)
                        (?S "sum" nil nil)
                        (?e "varepsilon" nil nil)
                        (?: "partial" nil nil)
			(?< "le" nil nil)
			(?> "ge" nil nil)
			(?, "int" nil nil)
			))
(require 'auctex)
(defun my-tex-config ()
  (iimage-mode 0)
  (auto-fill-mode 1)
  (setq auto-fill-function 'my-LaTeX-auto-fill-function)
  (TeX-PDF-mode 1)
  (TeX-source-correlate-mode 1)
  (LaTeX-math-mode 1)
  (local-set-key (kbd "C-c C-d") 'TeX-insert-braces)
  (local-set-key (kbd "s-c") 'my-latex-compile)
  (local-set-key (kbd "C-c s") (lambda () (interactive) (reftex-reference "s")))
  (local-set-key (kbd "C-c e") (lambda () (interactive) (reftex-reference "e")))
  (local-set-key (kbd "C-c f") (lambda () (interactive) (reftex-reference "f")))
  (local-set-key (kbd "C-c C-g") nil)
  (local-set-key (kbd "s-a") 'TeX-command-run-all)
  (local-set-key (kbd "C-c C-a") (lambda ()(interactive) (let ((LaTeX-default-environment "align")) (LaTeX-environment nil))))
  (local-set-key (kbd "s-p") (lambda ()(interactive)
			       (save-excursion
				 (search-backward "(")
				 (insert "\\left")
				 (search-forward ")")
				 (backward-char)
				 (insert "\\right"))))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  (setq LaTeX-beamer-item-overlay-flag nil)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-label-alist '(AMSTeX)) ;; eqref
  (setq reftex-ref-macro-prompt nil)
  (fset 'reftex-toc-quit 'reftex-toc-quit-and-kill)
  (turn-on-reftex)
  ;; (setq reftex-label-alist nil)

  ;; ;; undo TeX remaps, otherwise it interferes with compilation
  ;; (define-key TeX-mode-map [remap next-error] nil)
  ;; (define-key TeX-mode-map [remap previous-error] nil)

  ;; Try to guess a smart value for TeX-master
  ;; If the file contains local variables defining TeX-master, respect that.
  ;; Otherwise, look for a master file in the current directory
  ;; Define a local variable by
  ;; %%% Local Variables:
  ;; %%% TeX-master: "something"
  ;; %%% End:
  ;; list of master files to look for, increasing order of priority
  (setq list-of-master-files '("poly" "report" "master" "main"))
  ;; OK, this is a hack, but we force parsing of the file local variables here
  (hack-local-variables)
  ;; if a master file exists in the current directory, set it
  (unless (stringp TeX-master)
    (dolist (name list-of-master-files)
      (when (file-exists-p (concat name ".tex"))
  	(setq TeX-master name))))

  ;; indent align like equations
  (setq LaTeX-indent-environment-list
        '(("verbatim" current-indentation)
          ("verbatim*" current-indentation)
          ("tabular" LaTeX-indent-tabular)
          ("tabular*" LaTeX-indent-tabular)
          ("align")
          ("align*")
          ("array" LaTeX-indent-tabular)
          ("eqnarray" LaTeX-indent-tabular)
          ("eqnarray*" LaTeX-indent-tabular)
          ("displaymath")
          ("equation")
          ("equation*")
          ("picture")
          ("tabbing")))
  (LaTeX-add-environments "align")
  (LaTeX-add-environments "align*")

  (setq LaTeX-float "h!")

  (TeX-add-symbols '("left" nothing1)) ; bit of a hack, to avoid the left prompting from braces
  (defun nothing1 (a))
  (LaTeX-math-initialize))
(add-hook 'TeX-update-style-hook 'my-tex-config 'attheend)

(defun my-latex-compile ()
  "Run a special compile for latex files"
  (interactive)
  (setq my-latex-compiling-buffer (current-buffer))
  (let ((display-buffer-base-action nil))
    (compile
     (format
      "rubber -df %s"
      (if (stringp TeX-master)
          TeX-master
        (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))))

(defun my-after-latex-compile (buf stat)
  "Display viewer after compilation"
  (when (and (boundp 'my-latex-compiling-buffer)
	     (equal my-latex-compiling-buffer (window-buffer))
	     (equal stat "finished\n"))
    (with-current-buffer my-latex-compiling-buffer
      (let ((file (if (stringp TeX-master)
		      TeX-master
		    (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))
	(TeX-reader-sync-view) ; new in emacs 28
	;; put evince to front
        (shell-command-to-string
	 (format "wmctrl -r %s.pdf -t 3 && wmctrl -a %s.pdf"
		 file file)))

      (setq my-latex-compiling-buffer nil))))
(add-hook 'compilation-finish-functions 'my-after-latex-compile)

;; recenter and show after a sync
(defadvice TeX-source-correlate-sync-source (after AL/recenter-after-correlate activate)
  (shell-command-to-string
   (format "wmctrl -a %s"
           (shell-quote-argument (frame-parameter nil 'name))))
  (recenter))

(add-hook 'pdf-sync-backward-hook (lambda ()
                                    (interactive)
                                    (recenter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default comint-scroll-to-bottom-on-input 'all
	      comint-move-point-for-output t
	      comint-input-ring-file-name "~/.emacs.d/comint_history"
              comint-history-isearch 'dwim)
(ansi-color-for-comint-mode-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org-install)
;; (add-hook 'org-mode-hook 'auto-fill-mode)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-startup-indented t)
(global-set-key "\C-cl" 'org-store-link)

;;bindings
(require 'org)
(define-key org-mode-map (kbd "C-c C-r") 'org-refile)
(define-key org-mode-map (kbd "<C-tab>") nil)
(define-key org-mode-map (kbd "M-j") 'org-meta-return)

;;settings
(setq
 org-agenda-files (list "~/Dropbox/todo.org")
 org-default-notes-file "~/.emacs.d/org/notes.org"
 org-completion-use-ido t
 org-agenda-span 'week
 org-log-done 'time
 org-startup-folded 'content
 org-deadline-warning-days 4
 org-agenda-show-all-dates t
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-start-on-weekday 1
 calendar-week-start-day 1
 org-agenda-show-current-time-in-grid nil
 org-extend-today-until 4
 org-agenda-remove-tags t
 org-agenda-repeating-timestamp-show-all t
 org-agenda-use-time-grid nil
 org-reverse-note-order t
 org-capture-templates '(("t" "Scheduled task" entry
			  (file+headline "~/.emacs.d/org/todo.org" "Tasks")
			  "* TODO %?\nSCHEDULED: %t\n%a\n%i"))
 org-irc-link-to-logs t
 org-todo-keywords '((sequence "TODO" "WAITING" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" . font-lock-function-name-face) ("WAITING" . font-lock-keyword-face)
        ("DONE" . font-lock-string-face)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'compile)
;;make compile window disappear after successful compilation
(setq compilation-finish-function
      (lambda (buf str)
	(if (string-match "*Compilation*" (buffer-name buf))
	    (unless (string-match "abnormally" str)
	      ;;no errors, make the compilation window go away
	      (delete-windows-on buf)
	      (bury-buffer buf)))))

;;misc compilation settings
(setq-default
 compile-command "make"
 compilation-read-command nil
 compilation-scroll-output 'first-error
 compilation-ask-about-save nil
 compilation-window-height 10
 compilation-auto-jump-to-first-error nil
 compilation-disable-input t)

(setq compilation-message-face nil
      compilation-error-face compilation-line-face) ; tailored for julia-repl

;;compilation by C-c C-c in modes that don't shadow it
(global-set-key (kbd "C-c C-c") 'compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;see http://www.emacswiki.org/emacs/IgnacioKeyboardQuit , with a little bit of modifications
(defadvice keyboard-quit (around escape-from-minibuffer activate)
  "Escape the minibuffer or cancel region consistently using 'Control-g'.
Normally if the minibuffer is active but we lost focus (say, we clicked away or set the cursor into another buffer)
we can quit by pressing 'ESC' three times. This function handles it more conveniently, as it checks for the condition
of not being in the minibuffer but having it active. Otherwise simply doing the ESC or (keyboard-escape-quit) would
brake whatever split of windows we might have in the frame."
  (when (not(window-minibuffer-p (selected-window)))
    (when (active-minibuffer-window)
      (keyboard-escape-quit)))
  ad-do-it)

;; fix circumflex key on some french layouts
(global-set-key (kbd "<dead-circumflex> <dead-circumflex>") (kbd "^"))
;;find file at point
(global-set-key (kbd "<C-return>") 'ffap)
;;I just want C-x k to kill the buffer instead of just prompting me
;;for it
(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)
;;undo on C-z, because it's useless by default
(global-set-key (kbd "C-z") 'undo)
;;like C-x k, but nicer :-)
(global-set-key (kbd "C-x l") 'bury-buffer)
;;could not live without
(global-set-key (kbd "M-q") 'backward-kill-word)
(global-set-key (kbd "C-M-q") 'backward-kill-sexp)
(global-set-key (kbd "C-q") 'backward-delete-char)
;;rebind previous M-q binding to M-s
(global-set-key (kbd "M-s") 'fill-paragraph)
;;nice to have, coherent with other keybindings, and bound to nothing
;;by default, so ...
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
;;shortcuts for region commenting
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c c") 'comment-region)
;;switch between .c and .h
(global-set-key (kbd "C-c o") 'ff-find-other-file)
;;quite handy
(defun my-kill-whole-line ()
  (interactive)
  (let ((col (current-column)))
    (kill-whole-line 1)
    (move-to-column col)))
(global-set-key (kbd "C-S-k") 'my-kill-whole-line)
(setq kill-do-not-save-duplicates t)
;;sometimes useful (for query-replace and such)
(global-set-key (kbd "C-c C-SPC") 'transient-mark-mode)
;;easy window management for azerty keyboards. This can fail
;;on some terminals, so protect
(condition-case err
    (global-set-key (kbd "M-é") 'split-window-vertically)
  (error
   (message "Failed to bind M-é")))
(global-set-key (kbd "M-\"") 'split-window-horizontally)
(defun my-delete-other-windows ()
  "Delete other windows, and bury buffers that were displayed"
  (interactive)
  (when (> (length (window-list)) 1)
    (dolist (win (cdr (window-list)))
      (unless (equal (current-buffer) (window-buffer win))
	(bury-buffer (window-buffer win))))
    (delete-other-windows)))
(global-set-key (kbd "M-&") 'my-delete-other-windows)
(global-set-key (kbd "C-x 1") 'delete-other-windows)

;;make use of that useless ^2 key to do something useful. This can fail on some terminals,
;;so protect
(condition-case err
    (progn
      ;;normal
      (global-set-key (kbd "²") (lambda () (interactive) (insert "\\")))
      (global-set-key (kbd "œ") (lambda () (interactive) (insert "\\")))
      ;;isearch
      (define-key isearch-mode-map (kbd "²")
	(lambda ()
	  (interactive)
	  (if current-input-method
	      (isearch-process-search-multibyte-characters ?\\)
	    (isearch-process-search-char ?\\))))
      (define-key isearch-mode-map (kbd "œ")
	(lambda ()
	  (interactive)
	  (if current-input-method
	      (isearch-process-search-multibyte-characters ?\\)
	    (isearch-process-search-char ?\\)))))
  (error
   (message "Failed to bind key to \\.")))

;; replace $$ in M-! by the name of the associated buffer
(defun shell-command-replace (command &optional output-buffer error-buffer)
  "Same as shell-command, but replace occurences of $$ by the current buffer name"
  (interactive
   (list
    (read-shell-command "Shell command: " nil nil
			(and buffer-file-name
			     (file-relative-name buffer-file-name)))
    current-prefix-arg
    shell-command-default-error-buffer))
  (shell-command (replace-regexp-in-string "\\$\\$" (buffer-name (current-buffer-not-mini)) command)
		 output-buffer error-buffer))
(global-set-key (kbd "M-!") 'shell-command-replace)

;;zap to char -> zap up to char
;;found at emacs wiki, added the repeat part
(defun zap-up-to-char (arg char)
  "Kill up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive "p\ncZap up to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (kill-region (point)
		 (progn
		   (forward-char direction)
		   (unwind-protect
		       (search-forward (char-to-string char) nil nil arg)
		     (backward-char direction))
		   (point))))
  ;;repeat same key to repeat command. adapted code found in kmacro
  (message "Press %s to repeat" (char-to-string char))
  (if (equal char (read-event))
      (zap-up-to-char arg char)
    (setq unread-command-events (list last-input-event))))
(defun zap-up-to-char-back (char)
  (interactive "cBackward zap up to char: ")
  (zap-up-to-char -1 char))
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-M-z") 'zap-up-to-char-back)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Super keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to use on console environments, where s- just doesn't work.
;; seriously, when is the last time you used insert?
(define-key function-key-map (kbd "<insert>") 'event-apply-super-modifier)
(define-key function-key-map (kbd "<insertchar>") 'event-apply-super-modifier)
(define-key function-key-map (kbd "<f5>") 'event-apply-super-modifier)
(global-set-key (kbd "<insertchar>") nil)
(global-set-key (kbd "<insert>") nil)
(global-set-key (kbd "<f5>") nil)

;;shortcuts to two-keys commands I often use
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-b") 'switch-to-buffer)
(global-set-key (kbd "s-c") 'compile)
(global-set-key (kbd "s-f") 'find-file)
(global-set-key (kbd "s-u") 'undo)
(global-set-key (kbd "s-q") 'comment-line)
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))
(global-set-key (kbd "s-i") 'iwb)
(global-set-key (kbd "s-x") 'exchange-point-and-mark)
(global-set-key (kbd "s-SPC") 'pop-global-mark)
(global-set-key (kbd "s-;") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(defun kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters"
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
	(progn
	  (re-search-backward "[^ \t\r\n]" nil t)
	  (re-search-forward "[ \t\r\n]+" nil t)
	  (replace-match "" nil nil))))))
(global-set-key (kbd "s-k") 'kill-whitespace)
(defun open-shell-here ()
  (interactive)
  (launch-command "mate-terminal" ""))
(global-set-key (kbd "s-h") 'open-shell-here)

(defun open-file-explorer-here ()
  (interactive)
  (launch-command "xdg-open" "."))
(global-set-key (kbd "s-o") 'open-file-explorer-here)

(defun note ()
  (interactive)
  (find-file "~/Dropbox/notes.org"))
(defun todo ()
  (interactive)
  (find-file "~/Dropbox/todo.org"))
(global-set-key (kbd "s-n") 'note)
(global-set-key (kbd "s-t") 'todo)
(global-set-key (kbd "s-l") 'bury-buffer)
;; ghosts of past yanks
(global-set-key (kbd "s-y") (lambda ()
			      (interactive)
			      (popup-menu 'yank-menu)))
(global-set-key (kbd "s-m") 'mu4e-compose-new)
(defun duplicate-current-line ()
  (interactive)
  "Duplicate current line"
  (let ((text (buffer-substring (line-beginning-position) (line-end-position))))
    (save-excursion
      (end-of-line)
      (newline)
      (insert text))))
(global-set-key (kbd "s-d") 'duplicate-current-line)

;; increment/decrement
(defun add-digit-at-point (quantity)
  (or (looking-at "[0123456789]")
      (error "No number at point"))
  (save-excursion
    (replace-match (number-to-string (+ quantity (string-to-number (match-string 0)))))))

(defun multiply-number-at-point (quantity)
  (skip-chars-backward ".0123456789")
  (or (looking-at "[\\.0123456789]+")
      (error "No number at point"))
  (let ((num (* quantity (string-to-number (match-string 0)))))
    (when (= (floor num) num)
      (setq num (floor num)))
    (save-excursion
      (replace-match (number-to-string num)))))

(defun inc-at-point ()
  (interactive)
  (add-digit-at-point 1))
(defun dec-at-point ()
  (interactive)
  (add-digit-at-point -1))
(defun double-at-point ()
  (interactive)
  (multiply-number-at-point 2))
(defun halve-at-point ()
  (interactive)
  (multiply-number-at-point 0.5))
(global-set-key (kbd "<s-up>") 'inc-at-point)
(global-set-key (kbd "<s-down>") 'dec-at-point)
(global-set-key (kbd "<s-left>") 'halve-at-point)
(global-set-key (kbd "<s-right>") 'double-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Easy buffer switching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;quickly switch buffers
(defun switch-to-nth-buffer (n)
  "Switches to nth most recent buffer. Ignores a bunch of stuff."
  (catch 'tag
    (mapcar (lambda (b)
	      (unless
		  (or
		   (minibufferp b)
		   (string-match "^ " (buffer-name b))
		   (equal b (current-buffer)))
		(if (= n 1)
		    (progn
		      (switch-to-buffer b)
		      (throw 'tag nil))
		  (setq n (- n 1)))))
	    (buffer-list))))

(defun switch-to-most-recent-buffer ()
  (interactive)
  (switch-to-nth-buffer 1))
(defun switch-to-second-most-recent-buffer ()
  (interactive)
  (switch-to-nth-buffer 2))
(defun switch-to-third-most-recent-buffer ()
  (interactive)
  (switch-to-nth-buffer 3))

;;fast switching between two buffers
(global-set-key (kbd "<s-tab>") 'switch-to-most-recent-buffer)
(global-set-key (kbd "s-TAB") 'switch-to-most-recent-buffer)
;;fast switching between three buffers
(global-set-key (kbd "<C-tab>") 'switch-to-second-most-recent-buffer)
(global-set-key (kbd "<C-s-tab>") 'switch-to-third-most-recent-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc editing commands without keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;huge hack, but emacs internals are quite messy concerning
;;this. Don't even try to use regexps in the arguments :)
(defun query-exchange (str1 str2 &optional delimited start end)
  "Exchange str1 and str2 with a regexp replace"
  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Query exchange"
		   (if current-prefix-arg " word" "")
		   " regexp"
		   (if (and transient-mark-mode mark-active) " in region" ""))
	   t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   ;; These are done separately here
	   ;; so that command-history will record these expressions
	   ;; rather than the values they had this time.
	   (if (and transient-mark-mode mark-active)
	       (region-beginning))
	   (if (and transient-mark-mode mark-active)
	       (region-end)))))
  (defun my-aux-fun (match1 match2)
    (if (match-string 1) str2 str1))
  (defun my-add-word-boundary (str)
    (if current-prefix-arg (concat "\\<" str "\\>") str))
  (query-replace-regexp (format "\\(%s\\)\\|\\(%s\\)"
				(my-add-word-boundary str1)
				(my-add-word-boundary str2))
			'(my-aux-fun) delimited start end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ediff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;vertical split (terminology is confusing)
(setq ediff-split-window-function 'split-window-horizontally)
;;no separate frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;kill variants
(setq ediff-keep-variants nil)
;; restore window config on ediff, found on emacswiki, and modified to bury non-file buffers
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (setq my-ediff-bwin-config (current-window-configuration)))
;; this is a huge hack
(defun my-ediff-qh-before ()
  (setq my-ediff-buffer-A ediff-buffer-A
	my-ediff-buffer-B ediff-buffer-B
	my-ediff-buffer-C ediff-buffer-C))
(defun my-ediff-qh-after ()
  "Function to be called when ediff quits."
  (when (and my-ediff-buffer-A
	     (not (buffer-file-name my-ediff-buffer-A)))
    (bury-buffer my-ediff-buffer-A))
  (when (and my-ediff-buffer-B
	     (not (buffer-file-name my-ediff-buffer-B)))
    (bury-buffer my-ediff-buffer-B))
  (when (and my-ediff-buffer-C
	     (not (buffer-file-name my-ediff-buffer-C)))
    (bury-buffer my-ediff-buffer-C))
  (when my-ediff-bwin-config
    (set-window-configuration my-ediff-bwin-config)))
(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-quit-hook 'my-ediff-qh-before)
(add-hook 'ediff-quit-hook 'my-ediff-qh-after 'after)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Isearch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; match \n when searching spaces
(setq search-whitespace-regexp "[[:space:]\n]+")
(setq lazy-highlight-initial-delay 0) ; highlight matches without delay
(setq isearch-allow-scroll t)

;;zap to isearch
(defun zap-to-isearch ()
  (interactive)
  (kill-region isearch-opoint isearch-other-end)
  (isearch-done)
  (if (> isearch-other-end isearch-opoint)
      (backward-word)
    (forward-word)))

(define-key isearch-mode-map (kbd "M-z") 'zap-to-isearch)

;;C-o in isearch brings up every hit
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (isearch-exit)
      (occur (if isearch-regexp isearch-string
	       (regexp-quote isearch-string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dictionnaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq my-languages '("american" "french"))
(setq my-languages-index 0)
(defun icd ()
  "Cycle between dictionaries"
  (interactive)
  (setq my-languages-index (mod (+ my-languages-index 1) (length my-languages)))
  (setq ispell-dictionary (nth my-languages-index my-languages))
  (message ispell-dictionary))
;;english dictionary, change it with M-x ispell-change-dictionary or M-x icd
(setq ispell-dictionary "american"
      ispell-silently-savep t
      ispell-program-name "aspell")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tab completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tab-always-indent 'complete)

(defun my-dabbrev-expand ()
  "Expand using dabbrev, with a few safeguards"
  (interactive)
  (when (and (not (bolp))
	     (looking-at "\\_>"))
    (dabbrev-expand nil)))

(defun completion-at-point-using-dabbrev ()
  'my-dabbrev-expand)

(setq-default completion-at-point-functions '(completion-at-point-using-dabbrev))

(setq completion-show-inline-help nil)
(setq completion-ignore-case t)

					; prefer not forking out windows
(setq display-buffer-base-action '(display-buffer-same-window . nil))

(when emacs-is-master
  (set-frame-parameter nil 'fullscreen 'fullboth))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(remove-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)

(load-theme 'material t)
(require 'outline)
(set-face-attribute 'outline-1 nil :foreground "SkyBlue1")
(set-face-attribute 'outline-2 nil :foreground "CadetBlue1")
(set-face-attribute 'outline-3 nil :foreground "LightSteelBlue1")
(set-face-attribute 'outline-4 nil :foreground "turquoise2")
(set-face-attribute 'outline-5 nil :foreground "aquamarine1")
(set-face-attribute 'outline-6 nil :foreground "aquamarine2")
(set-face-attribute 'outline-7 nil :foreground "aquamarine3")
(set-face-attribute 'outline-8 nil :foreground "aquamarine4")

(setq visible-bell nil)

;;; wuxch-dired-copy-paste.el
(require 'dired)
(require 'dired-aux)

(defvar dired-copied-cutted-files-pool nil "global variable to store copied or cutted files")
(defvar dired-is-copied nil "t:copy  nil:cut")

(defun wuxch-dired-copy()
  ""
  (interactive)
  (wuxch-dired-do-copy-cut t)
  )

(defun wuxch-dired-cut()
  ""
  (interactive)
  (wuxch-dired-do-copy-cut nil)
  )

(defun wuxch-dired-do-copy-cut(is-copy)
  "wuxch-dired-do-copy-cut:"
  (wuxch-clear-copied-cutted-files-pool)
  (wuxch-put-marked-files-name-to-pool)
  (let ((copy-cut-string)(num (safe-length dired-copied-cutted-files-pool)))
    (setq dired-is-copied is-copy)
    (if is-copy
        (setq copy-cut-string "copied")
      (setq copy-cut-string "cut")
      )
    (if (eq num 1)
        (progn
          (message "%s is %s" (car dired-copied-cutted-files-pool) copy-cut-string)
          )
      (progn
        (message "%d file/dir(s) %s" num copy-cut-string)
        )
      )
    )
  )
(defun wuxch-dired-paste()
  "wuxch-dired-paste:"
  (interactive)
  (if (not (eq dired-copied-cutted-files-pool nil))
      (let ((copy-cut-string)(current-file-number 0)(file-number (safe-length dired-copied-cutted-files-pool)))
        (if dired-is-copied
            (setq copy-cut-string "copied")
          (setq copy-cut-string "moved"))
        (dolist (src-file dired-copied-cutted-files-pool)
          (let ((dst-file))
            (setq dst-file (concat (dired-current-directory) (file-name-nondirectory src-file)))
            (if dired-is-copied
                (dired-copy-file src-file dst-file t)
              (dired-rename-file src-file dst-file t)
              )
	    ;; MODIFIED simply revert buffer, without anything fancy
            ;; revert buffer.
	    (revert-buffer)
                                        ; MODIFIED don't mark
                                        ;(dired-mark-files-regexp (file-name-nondirectory src-file))
            ;; show some information
            (setq current-file-number (+ current-file-number 1))
            (message "%d of %d file/dir(s) %s" current-file-number file-number copy-cut-string)
            )
          )
        (if (not dired-is-copied)
            (wuxch-clear-copied-cutted-files-pool))
        )
    )
  )

(defun wuxch-clear-copied-cutted-files-pool()
  "wuxch-clear-copied-cutted-files-pool: clear the pool if it's not nil"
  (if (not (eq dired-copied-cutted-files-pool nil))
      (progn
        (setq dired-copied-cutted-files-pool nil)
        )
    )
  )

(defun wuxch-put-marked-files-name-to-pool()
  "wuxch-put-marked-files-name-to-pool:"
  (let ((files))
    (setq files (dired-get-marked-files t))
    (if (listp files)
        (dolist (element files)
          (setq dired-copied-cutted-files-pool
                (append dired-copied-cutted-files-pool (list (concat (dired-current-directory) element))))
          )
      )
    )
  )

(define-key dired-mode-map (kbd "M-w") 'wuxch-dired-copy)
(define-key dired-mode-map (kbd "C-w") 'wuxch-dired-cut)
(define-key dired-mode-map (kbd "C-y") 'wuxch-dired-paste)


;; automatically indent yanked text if in programming-modes : found somewhere on the net
(defvar yank-indent-modes '(emacs-lisp-mode python-mode
					    c-mode c++-mode
					    tcl-mode sql-mode
					    perl-mode cperl-mode
					    java-mode jde-mode
					    lisp-interaction-mode
					    scheme-mode
					    LaTeX-mode TeX-mode
					    matlab-mode ada-mode
					    fortran-mode f90-mode)
  "Modes in which to indent regions that are yanked (or yank-popped)")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(require 'magit)
(require 'forge)
(global-set-key (kbd "C-x v s") 'magit-status)
(global-set-key (kbd "C-x v p") 'magit-push-implicitly)
(global-set-key (kbd "C-x v f") 'magit-pull-from-upstream)

(setq magit-status-sections-hook
      '(magit-insert-status-headers magit-insert-merge-log magit-insert-rebase-sequence magit-insert-am-sequence magit-insert-sequencer-sequence magit-insert-bisect-output magit-insert-bisect-rest magit-insert-bisect-log magit-insert-unstaged-changes magit-insert-staged-changes magit-insert-stashes magit-insert-unpulled-from-upstream magit-insert-unpulled-from-pushremote magit-insert-unpushed-to-upstream magit-insert-unpushed-to-pushremote))

(define-key magit-status-mode-map (kbd "<s-tab>") nil)
(define-key magit-status-mode-map (kbd "<C-tab>") nil)
(define-key magit-status-mode-map (kbd "<C-s-tab>") nil)

(setq magit-diff-refine-hunk 'all)

(setq diff-refine 'font-lock)

(when (get-buffer "*scratch*") (kill-buffer "*scratch*"))
(setq initial-buffer-choice "~/")

(setq uniquify-strip-common-suffix nil)

(ivy-mode 1)
(setq magit-completing-read-function 'ivy-completing-read)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "")
(setq ivy-initial-inputs-alist nil)
(setq ivy-sort-matches-functions-alist '(t))
(setq ivy-virtual-abbreviate 'full)

(global-set-key (kbd "M-x") 'counsel-M-x)
(ivy-configure 'counsel-M-x
  :initial-input "^"
  :display-transformer-fn #'counsel-M-x-transformer)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "s-f") 'counsel-find-file)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-g g") 'counsel-ag)
(setq ivy-more-chars-alist (list (cons t 2)))
(global-set-key (kbd "C-x C-r") 'counsel-recentf)
(global-set-key (kbd "M-y") 'counsel-yank-pop)

(global-set-key (kbd "M-g M-g") 'avy-goto-line)
(require 'avy)
(define-key isearch-mode-map (kbd "M-g") 'avy-isearch)
(setq avy-keys '(?q 115 100 102 103 104 106 107 108)) ;azerty keyboard

(require 'highlight-indentation)
(add-hook 'python-mode-hook 'highlight-indentation-mode)

(remove-mm-lighter 'ivy-mode)
(remove-mm-lighter 'magic-latex-buffer)
(remove-mm-lighter 'iimage-mode)
(remove-mm-lighter 'eldoc-mode)
(require 'reftex)
(remove-mm-lighter 'reftex-mode)
(remove-mm-lighter 'visual-line-mode)
(remove-mm-lighter 'highlight-indentation-mode)
(require 'flx)

(define-key
  ivy-switch-buffer-map
  (kbd "C-k")
  (lambda ()
    (interactive)
    (kill-buffer ivy--current)
    ;;(ivy--reset-state ivy-last)
    ))


;; Until I can be bothered to use the new gnus viewer
(setq mu4e-view-use-old t)
;; mu4e
(setq
 mu4e-maildir       "~/.emacs.d/mbsync"   ;; top-level Maildir
 mu4e-sent-folder   "/[Google Mail]/.Sent Mail"       ;; folder for sent messages
 mu4e-drafts-folder "/[Google Mail]/.Drafts"     ;; unfinished messages
 mu4e-get-mail-command "timeout 30 mbsync all; true")
(load "~/.emacs.d/priv_mu4e.el") ;; set private variables

(require 'timezone)
(setq mu4e-use-fancy-chars t
      mu4e-update-interval 40
      mu4e-view-show-images t
      mu4e-view-image-max-width 800
      mu4e-sent-messages-behavior 'delete
      mu4e-hide-index-messages t
      mu4e-completing-read-function 'ivy-completing-read
      mu4e-compose-complete-only-personal nil
      mu4e-headers-fields '((:human-date . 6)
                            ;; (:maildir . 10)
                            (:from-or-to . 22)
                            (:subject . 100))
      mu4e-headers-time-format "%R"
      mu4e-headers-date-format "%d/%m"
      mu4e-headers-auto-update nil
      mu4e-view-show-addresses nil
      mu4e-compose-dont-reply-to-self t
      mu4e-headers-include-related nil
      mu4e-headers-results-limit 100
      mu4e-view-fill-headers nil
      mu4e-compose-auto-include-date t
      mu4e-change-filenames-when-moving t
      mu4e-headers-leave-behavior 'apply
      mu4e-headers-show-threads nil
      mu4e-save-multiple-attachments-without-asking t
      shr-color-visible-luminance-min 40

      message-citation-line-format "\n%d %B %Y %R %Z, %f:"
      message-generate-new-buffers 'unique
      message-wash-forwarded-subjects t
      message-citation-line-function (lambda () (message-insert-formatted-citation-line nil nil (* 60 (timezone-zone-to-minute (current-time-zone))))) ; don't use the sender's timezone
      message-kill-buffer-on-exit t
      message-send-mail-function 'message-smtpmail-send-it ; can also do it async if needed, with smtpmail-async
      starttls-use-gnutls t
      starttls-gnutls-program "gnutls-cli"
      )

;; Special dance to make it use different SMTP servers according to sender
(require 'cl)
(require 'smtpmail)
(setq smtp-accounts
      '(
	("." "smtp.gmail.com" "antoine.levitt@gmail.com" 587 nil)
	("antoine.levitt@universite-paris-saclay.fr" "smtps.universite-paris-saclay.fr" "antoine.levitt" 465 ssl)
	))
(defun my-change-smtp ()
  (save-excursion
    (loop with from = (save-restriction
                        (message-narrow-to-headers)
                        (message-fetch-field "from"))
          for (addr server user port stream) in smtp-accounts
          when (string-match addr from)
          do (progn (setq smtpmail-smtp-user user
			  smtpmail-smtp-server server
			  smtpmail-smtp-service port
			  smtpmail-stream-type stream
			  )))))
(defadvice smtpmail-via-smtp
    (before change-smtp-by-message-from-field (recipient buffer &optional ask) activate)
  (with-current-buffer buffer (my-change-smtp)))

;; gmail saves copies automatically, but not the others
(setq mu4e-sent-messages-behavior
      (lambda ()
	(if (string= (message-sendmail-envelope-from) "antoine.levitt@universite-paris-saclay.fr")
	    'sent 'delete)))

(require 'mu4e)

;; I don't like ellipsis in headers
(defun mu4e~headers-field-truncate-to-width (_msg _field val width)
  "Truncate VAL to WIDTH."
  (if width
      (truncate-string-to-width val width 0 ?\s " ")
    val))

;; filter headers to remove the [Google Mail]
(setq mu4e~headers-field-handler-functions
      '(AL-filter-headers
        mu4e~headers-field-apply-basic-properties
        mu4e~headers-field-truncate-to-width))
(defun AL-filter-headers (_msg _field val width)
  (if (eq :maildir _field)
      (progn
        (cond ((string-match "Drafts" val) ".Drafts")
              ((string-match "Sent" val) ".Sent")
              (t (substring val 1 nil))))
    val))

(add-to-list 'mu4e-compose-hidden-headers "^In-Reply-To:")
(add-to-list 'mu4e-compose-hidden-headers "^MIME-Version:")
(add-to-list 'mu4e-compose-hidden-headers "^Received:")
(add-to-list 'mu4e-compose-hidden-headers "^X-TUID:")
(setq mu4e-view-fields '(:subject :from :to :cc  :date :attachments))
(setq mu4e-view-fields '(:from :to :cc :subject :date :mailing-list :user-agent :attachments))
(setq gnus-visible-headers "^Newsgroups:\\|^Subject:\\|^From:\\|^Date:\\|^Followup-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:")
(add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser) t)

(require 'mu4e-alert)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
(setq alert-default-style 'libnotify)
(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(setq mu4e-alert-email-notification-types '(subjects))
(setq mu4e-alert-interesting-mail-query "flag:unread AND (maildir:/INBOX OR maildir:/InriaBox/INBOX OR maildir:/OrsayBox/INBOX)")
(setq mu4e-alert-set-window-urgency nil)
(setq mu4e-alert-max-messages-to-process 50)
(defun mu4e-alert-default-grouped-mail-notification-formatter (mail-group all-mails)
  "Default function to format MAIL-GROUP for notification.

ALL-MAILS are the all the unread emails"
  (let* ((mail-count (length mail-group))
         (total-mails (length all-mails))
         (first-mail (car mail-group))
         (title-prefix "")
         (field-value (mu4e-alert--get-group first-mail))
         (title-suffix (format "%s"
                               field-value))
         (title (format "%s %s\n" title-prefix title-suffix)))
    (list :title title
          :body (concat " "
                        (s-join "\n"
                                (mapcar (lambda (mail)
                                          (plist-get mail :subject))
                                        mail-group))))))
(setq AL-mail-count 0)
(defun mu4e-alert-default-mode-line-formatter (mail-count)
  "AL: My version, don't display the count but save it"
  (setq AL-mail-count mail-count)
  (when (not (zerop mail-count))
    (concat " "
            (propertize
             "Mail"
             'display (when (display-graphic-p)
                        display-time-mail-icon)
             'face display-time-mail-face
             'help-echo (concat (if (= mail-count 1)
                                    "You have an unread email"
                                  (format "You have %s unread emails" mail-count))
                                "\nClick here to view "
                                (if (= mail-count 1) "it" "them"))
             'mouse-face 'mode-line-highlight
             'keymap '(mode-line keymap
                                 (mouse-1 . mu4e-alert-view-unread-mails)
                                 (mouse-2 . mu4e-alert-view-unread-mails)
                                 (mouse-3 . mu4e-alert-view-unread-mails)))
            " "
            )))


;;;; The default behavior is to mark and then go to the next message, which I don't like.
(defmacro mu4e~headers-defun-mark-for (mark)
  "Define a function mu4e~headers-mark-MARK."
  (let ((funcname (intern (format "mu4e-headers-mark-for-%s" mark)))
	(docstring (format "Mark header at point with %s." mark)))
    `(progn
       (defun ,funcname () ,docstring
              (interactive)
              (mu4e-mark-set ',mark))
       (put ',funcname 'definition-name ',mark))))
(mu4e~headers-defun-mark-for refile)
(mu4e~headers-defun-mark-for something)
(mu4e~headers-defun-mark-for delete)
(mu4e~headers-defun-mark-for flag)
(mu4e~headers-defun-mark-for move)
(mu4e~headers-defun-mark-for read)
(mu4e~headers-defun-mark-for trash)
(mu4e~headers-defun-mark-for unflag)
(mu4e~headers-defun-mark-for untrash)
(mu4e~headers-defun-mark-for unmark)
(mu4e~headers-defun-mark-for unread)
(mu4e~headers-defun-mark-for action)
(defmacro mu4e~view-defun-mark-for (mark)
  "Define a function mu4e-view-mark-for-MARK."
  (let ((funcname (intern (format "mu4e-view-mark-for-%s" mark)))
	(docstring (format "Mark the current message for %s." mark)))
    `(progn
       (defun ,funcname () ,docstring
              (interactive)
              (mu4e~view-in-headers-context
               (mu4e-mark-set ',mark)))
       (put ',funcname 'definition-name ',mark))))
(mu4e~view-defun-mark-for move)
(mu4e~view-defun-mark-for trash)
(mu4e~view-defun-mark-for refile)
(mu4e~view-defun-mark-for delete)
(mu4e~view-defun-mark-for flag)
(mu4e~view-defun-mark-for unflag)
(mu4e~view-defun-mark-for unmark)
(mu4e~view-defun-mark-for something)
(mu4e~view-defun-mark-for read)
(mu4e~view-defun-mark-for unread)

(define-key mu4e-main-mode-map (kbd "q") 'bury-buffer) ;never quit
(define-key mu4e-headers-mode-map (kbd "SPC") 'mu4e-headers-view-message)
(define-key mu4e-headers-mode-map (kbd "d") 'mu4e-headers-mark-for-delete)
(define-key mu4e-view-mode-map (kbd "d") 'mu4e-view-mark-for-delete)
(define-key mu4e-headers-mode-map (kbd "i") 'mu4e-headers-mark-for-unread)
(define-key mu4e-view-mode-map (kbd "i") 'mu4e-view-mark-for-unread)

(define-key mu4e-view-mode-map (kbd "r") 'mu4e-compose-reply)
(define-key mu4e-view-mode-map (kbd "f") 'mu4e-compose-forward)

(define-key mu4e-main-mode-map (kbd "c") 'mu4e-compose-new)
(define-key mu4e-main-mode-map (kbd "r") (lambda () (interactive) (mu4e-headers-search "flag:unread AND (maildir:/INBOX OR maildir:/InriaBox/INBOX OR maildir:/OrsayBox/INBOX)" nil nil t nil nil)))
(define-key mu4e-main-mode-map (kbd "i") (lambda () (interactive) (mu4e-headers-search "(maildir:/INBOX OR maildir:/InriaBox/INBOX OR maildir:/OrsayBox/INBOX)" nil nil t)))
(define-key mu4e-main-mode-map (kbd "g") (lambda () (interactive) (mu4e-headers-search "flag:unread" nil nil t nil nil)))
(define-key mu4e-main-mode-map (kbd "d") (lambda () (interactive) (mu4e-headers-search "m:/\"[Google Mail]/.Drafts\"" nil nil t nil t)))
(define-key mu4e-main-mode-map (kbd "s") (lambda () (interactive) (mu4e-headers-search "m:/\"[Google Mail]/.Sent Mail\" OR m:/InriaBox/Sent OR m:/OrsayBox/Sent" nil nil t)))
(define-key mu4e-main-mode-map (kbd "q") (lambda () (interactive) (mu4e-headers-search)))
(define-key mu4e-main-mode-map (kbd "a") (lambda () (interactive) (mu4e-headers-search "" nil nil t)))
(global-set-key (kbd "s-e") mu4e-main-mode-map)

(global-set-key (kbd "s-r") (lambda () (interactive) (when (> AL-mail-count 0) (mu4e-headers-search "flag:unread AND (maildir:/INBOX OR maildir:/InriaBox/INBOX OR maildir:/OrsayBox/INBOX)" nil nil t nil nil)))) ; last t: open first message
(global-set-key (kbd "s-g") (lambda () (interactive) (mu4e-headers-search "flag:unread" nil nil t nil t))) ; last t: open first message
(global-set-key (kbd "s-g") (lambda () (interactive) (mu4e-headers-search "flag:unread" nil nil t nil nil))) ; last t: open first message
(global-set-key (kbd "s-v") (lambda () (interactive) (mu4e-headers-search "flag:unread AND NOT (maildir:/INBOX OR maildir:/InriaBox/INBOX OR maildir:/OrsayBox/INBOX)" nil nil t nil nil)))
(defun mu4e--main-view (&optional refresh) nil) ;; too extreme? Bof.
(global-set-key (kbd "C-x m") 'mu4e-compose-new)
(define-key mu4e-compose-mode-map (kbd "M-q") nil)
(define-key mu4e-compose-mode-map (kbd "M-n") nil)

;; redefine Subject header face
(defun mu4e~compose-remap-faces ()
  "Our parent `message-mode' uses font-locking for the compose
buffers; lets remap its faces so it uses the ones for mu4e."
  ;; normal headers
  (face-remap-add-relative 'message-header-name
                           '((:inherit mu4e-header-key-face)))
  (face-remap-add-relative 'message-header-other
                           '((:inherit mu4e-header-value-face)))
  ;; special headers
  (face-remap-add-relative 'message-header-from
                           '((:inherit mu4e-contact-face)))
  (face-remap-add-relative 'message-header-to
                           '((:inherit mu4e-contact-face)))
  (face-remap-add-relative 'message-header-cc
                           '((:inherit mu4e-contact-face)))
  (face-remap-add-relative 'message-header-bcc
                           '((:inherit mu4e-contact-face)))
  (face-remap-add-relative 'message-header-subject
                           '((:inherit font-lock-string-face)))
  ;; citation
  (face-remap-add-relative 'message-cited-text
                           '((:inherit mu4e-cited-1-face))))

(mu4e)

(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
(define-key dired-mode-map (kbd "a") 'gnus-dired-attach)

;; Update date when saving a draft
(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'mu4e~draft-update-date nil t)))
(defun mu4e~draft-update-date ()
  (save-excursion
    (message-position-on-field "Date")
    (message-beginning-of-line)
    (kill-line)
    (insert (message-make-date))))

(setq gnus-article-date-headers '(local))

;; https://github.com/sje30/emacs/blob/master/mu4e-view-save-all-attachments.el
;; modified to also open dired and overwrite the files instead of renaming them
;; when called multiple times
(defvar bulk-saved-attachments-dir "/tmp/mu4e")

(define-key mu4e-view-mode-map ">" 'mu4e-view-save-all-attachments)
(defun cleanse-subject (sub)
  (replace-regexp-in-string
   "[^A-Z0-9]+"
   "-"
   (downcase sub)))

(defun mu4e-view-save-all-attachments (&optional arg)
  "Save all MIME parts from current mu4e gnus view buffer."
  ;; Copied from mu4e-view-save-attachments
  (interactive "P")
  (cl-assert (and (eq major-mode 'mu4e-view-mode)
                  (derived-mode-p 'gnus-article-mode)))
  (let* ((msg (mu4e-message-at-point))
         (id (cleanse-subject (mu4e-message-field msg :subject)))
         (attachdir (concat bulk-saved-attachments-dir "/" id))
	 (parts (mu4e~view-gather-mime-parts))
         (handles '())
         (files '())
         dir)
    (mkdir attachdir t)
    (dolist (part parts)
      (let ((fname (or
		    (cdr (assoc 'filename (assoc "attachment" (cdr part))))
                    (seq-find #'stringp
                              (mapcar (lambda (item) (cdr (assoc 'name item)))
                                      (seq-filter 'listp (cdr part)))))))
        (when fname
          (push `(,fname . ,(cdr part)) handles)
          (push fname files))))
    (if files
        (progn
          (setq dir
		(if arg (read-directory-name "Save to directory: ")
		  attachdir))
          (cl-loop for (f . h) in handles
                   when (member f files)
                   do (mm-save-part-to-file h
					    (expand-file-name f dir)))
	  (dired dir))
      (mu4e-message "No attached files found"))))


(require 'iedit)

(add-hook 'reftex-select-label-mode-hook 'reftex-reparse-document)

(require 'julia-repl)
(setq julia-repl-executable-records
      '((default "/home/antoine/.juliaup/bin/julia")))
(setq julia-repl-switches "-t 6")
(setq julia-repl-save-buffer-on-send t)

(setq AL/last-sent-julia-buffer nil)
(defun AL/julia-repl-send-buffer (arg)
  (interactive "P")
  (setq AL/last-sent-julia-buffer buffer-file-name)
  (julia-repl-send-buffer arg))
(defun AL/julia-repl-send-last-command (arg)
  (interactive "P")
  (if AL/last-sent-julia-buffer
      (progn
	(save-buffer)
	(julia-repl--send-string
         (concat "include(\""
                 (julia-repl--path-rewrite AL/last-sent-julia-buffer julia-repl-path-rewrite-rules)
                 "\");")))
    (AL/julia-repl-send-buffer)))

(define-key julia-repl-mode-map (kbd "s-a") 'AL/julia-repl-send-last-command)
(define-key julia-repl-mode-map (kbd "C-c C-c") 'AL/julia-repl-send-buffer)
(define-key julia-repl-mode-map (kbd "C-c C-l") 'julia-repl-send-region-or-line)
(define-key julia-repl-mode-map (kbd "<C-return>") nil)
(add-hook 'julia-mode-hook 'julia-repl-mode)
(require 'julia-repl)
(require 'vterm)
(define-key vterm-mode-map (kbd "C-v") nil)
(define-key vterm-mode-map (kbd "M-v") nil)
(julia-repl-set-terminal-backend 'vterm)
(setq vterm-max-scrollback 10000)

(global-set-key (kbd "s-j") 'julia-repl)
(global-set-key (kbd "s-z") (kbd "C-c C-z"))
(global-set-key (kbd "s-c") (kbd "C-c C-c"))

;; Force one-window setup
(setq display-buffer-alist
      '(("*julia" . ((display-buffer-same-window) (inhibit-same-window . nil)))
        ("\\.jl$" . ((display-buffer-same-window) (inhibit-same-window . nil)))))

;; (setq kill-buffer-query-functions nil)

;; get dead buffers out of the way, but keep them around in case I need them
(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (rename-buffer "*ansi-term* (dead)" t)
  (bury-buffer))

(defun visit-term-buffer (arg)
  "Create or visit a terminal buffer."
  (interactive "P")
  (if (or arg
          (not (get-buffer "*ansi-term*")))
      (ansi-term (getenv "SHELL"))
    (switch-to-buffer "*ansi-term*")))

;; (global-set-key (kbd "s-h") 'visit-term-buffer)

(setq term-scroll-to-bottom-on-output t)
(define-key term-mode-map (kbd "s-c")
  (lambda () (interactive) (if (term-in-char-mode)
			       (term-line-mode)
			     (term-char-mode))))
(define-key term-raw-map (kbd "s-c")
  (lambda () (interactive) (if (term-in-char-mode)
			       (term-line-mode)
			     (term-char-mode))))
(define-key term-mode-map (kbd "C-c C-c") (lambda () (interactive) (term-char-mode)))
(define-key term-raw-map (kbd "C-v") nil)
(define-key term-raw-map (kbd "M-v") nil)
(define-key term-raw-map (kbd "M-<") nil)
(define-key term-raw-map (kbd "M->") nil)
(define-key term-raw-map (kbd "C-y") nil)
(add-hook 'term-mode-hook #'eterm-256color-mode)



(defun visit-term-buffer (arg)
  "Create or visit a terminal buffer."
  (interactive "P")
  (if (or arg
          (not (get-buffer "vterm")))
      (vterm)
    (switch-to-buffer "vterm")))
(global-set-key (kbd "s-h") 'visit-term-buffer)

(require 'eterm-256color)

;; quit mu4e when composing a reply
(add-hook 'mu4e-compose-mode-hook
          (defun AL/full-window () (interactive)
                 (delete-other-windows)
                 (when (get-buffer "*mu4e-view*")
                   (with-current-buffer "*mu4e-view*"
                     (mu4e~view-quit-buffer)))
                 (when (get-buffer "*mu4e-headers*")
                   (with-current-buffer "*mu4e-headers*"
                     (mu4e~headers-quit-buffer)))))


;; immediately quit empty header buffers
(add-hook 'mu4e-headers-found-hook (lambda () (interactive)
				     (when (eq major-mode 'mu4e-headers-mode)
				       (save-excursion
					 (goto-char (point-min))
					 (end-of-visual-line)
					 (when (eobp)
					   (mu4e~headers-quit-buffer))))))

;; For the layout now called (apparently) "French French (legacy, alt.)", aka latin9 for setxkbmap
(setq AL/algr-keys   "å€þý¶ÂøÊ±æðÛÎÔ¹«»©®ß¬")
(setq AL/normal-keys "aetypqsdfghjklmwxcvbn")
(defun AL/map-keys (src dest)
  (when src
    (define-key key-translation-map (char-to-string (car src)) (kbd (concat "C-" (char-to-string (car dest)))))
    (AL/map-keys (cdr src) (cdr dest))))
(AL/map-keys (string-to-list AL/algr-keys)(string-to-list AL/normal-keys))
(define-key key-translation-map (kbd " ") (kbd "C-SPC"))

(require 'smartparens-config)
(smartparens-global-mode 1)
(remove-mm-lighter 'smartparens-mode)


(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)
(define-key smartparens-mode-map (kbd "M-)")    'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "M-(")    'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-)")    'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-(")    'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "M-S")    'sp-splice-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-a")    'sp-backward-up-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-e")    'sp-up-sexp)

;; (define-key smartparens-mode-map (kbd "C-M-t")    'sp-transpose-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-t")    'transpose-sexps)
(setq sp-highlight-pair-overlay nil)
;; (define-key smartparens-mode-map (kbd "M-q")    'sp-backward-kill-word)
;; (define-key smartparens-mode-map (kbd "M-d")    'sp-kill-word)
;; (add-to-list 'sp-no-reindent-after-kill-modes 'latex-mode)

(require 'cl)
(defmacro def-pairs (pairs)
  `(progn
     ,@(loop for (key . val) in pairs
             collect
             `(defun ,(read (concat
                             "wrap-with-"
                             (prin1-to-string key)
                             "s"))
                  (&optional arg)
		(interactive "p")
		(sp-wrap-with-pair ,val)))))

(def-pairs ((paren . "(")
            (bracket . "[")
            (brace . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote . "`")))

(define-key smartparens-mode-map (kbd "C-c (")    'wrap-with-parens)
(define-key smartparens-mode-map (kbd "C-c [")    'wrap-with-brackets)
(define-key smartparens-mode-map (kbd "C-c {")    'wrap-with-braces)


;; TODO C-M-p/n find begin/end LaTeX-find-matching-begin

(require 'latex nil t)

;; OK, now we overwrite this function to get rid of the "*" in the
;; interactive definition of the function that calls LaTeX-math-insert
;; and forbids us to use it in term-char-mode
(defun LaTeX-math-initialize ()
  (let ((math (reverse (append LaTeX-math-list LaTeX-math-default)))
	(map LaTeX-math-keymap)
	(unicode (and LaTeX-math-menu-unicode (fboundp 'decode-char))))
    (while math
      (let* ((entry (car math))
	     (key (nth 0 entry))
	     (prefix
	      (and unicode
		   (nth 3 entry)))
	     value menu name)
	(setq math (cdr math))
	(if (and prefix
		 (setq prefix (decode-char 'ucs (nth 3 entry))))
	    (setq prefix (concat (string prefix) " \\"))
	  (setq prefix "\\"))
	(if (listp (cdr entry))
	    (setq value (nth 1 entry)
		  menu (nth 2 entry))
	  (setq value (cdr entry)
		menu nil))
	(if (stringp value)
	    (progn
	      (setq name (intern (concat "LaTeX-math-" value)))
	      (fset name (list 'lambda (list 'arg) (list 'interactive "P")
			       (list 'LaTeX-math-insert value 'arg))))
	  (setq name value))
	(if key
	    (progn
	      (setq key (cond ((numberp key) (char-to-string key))
			      ((stringp key) (read-kbd-macro key))
			      (t (vector key))))
	      (define-key map key name)))
	(if menu
	    (let ((parent LaTeX-math-menu))
	      (if (listp menu)
		  (progn
		    (while (cdr menu)
		      (let ((sub (assoc (car menu) LaTeX-math-menu)))
			(if sub
			    (setq parent sub)
			  (setcdr parent (cons (list (car menu)) (cdr parent))))
			(setq menu (cdr menu))))
		    (setq menu (car menu))))
	      (let ((sub (assoc menu parent)))
		(if sub
		    (if (stringp value)
			(setcdr sub (cons (vector (concat prefix value)
						  name t)
					  (cdr sub)))
		      (error "Cannot have multiple special math menu items"))
		  (setcdr parent
			  (cons (if (stringp value)
				    (list menu (vector (concat prefix value)
						       name t))
				  (vector menu name t))
				(cdr parent)))))))))
    ;; Make the math prefix char available if it has not been used as a prefix.
    (unless (lookup-key map (LaTeX-math-abbrev-prefix))
      (define-key map (LaTeX-math-abbrev-prefix) 'self-insert-command))))
(LaTeX-math-initialize)

(require 'julia-mode)
;; Math insertion
(defun julia-math-insert (s)
  "Inserts math symbol given by `s'"
  (when s
    (let ((sym (gethash (concat "\\" s) julia-mode-latexsubs)))
      (when sym
	(cond ((and (eq major-mode 'term-mode) (term-in-char-mode)) (term-send-raw-string sym))
	      ((and (eq major-mode 'vterm-mode)) (vterm-insert sym))
	      (t (insert sym)))))))

;; unicode insertion of math symbols
(define-minor-mode julia-math-mode
  "A minor mode with easy access to TeX math commands. The
command is only entered if it is supported in Julia. The
following commands are defined:

\\{LaTeX-math-mode-map}"
  nil nil (list (cons (LaTeX-math-abbrev-prefix) LaTeX-math-keymap))
  (if julia-math-mode
      (set (make-local-variable 'LaTeX-math-insert-function) 'julia-math-insert)))

(defun in-latexp ()
  (or (eq major-mode 'latex-mode)
      (eq major-mode 'LaTeX-mode)))

(define-globalized-minor-mode global-math-mode julia-math-mode
  (lambda ()
    (unless (in-latexp)
      (julia-math-mode))))
(global-math-mode 1)

;; Make math insertion work in isearch
(add-hook 'isearch-mode-hook 'isearch-setup-latex-math)
(add-hook 'isearch-mode-end-hook 'isearch-unsetup-latex-math)
(define-key isearch-mode-map (LaTeX-math-abbrev-prefix) LaTeX-math-keymap)

(defun isearch-setup-latex-math ()
  (if (in-latexp)
      (set (make-local-variable 'LaTeX-math-insert-function) (lambda (s) (isearch-process-search-string (concat "\\" s) (concat "\\" s))))
    (set (make-local-variable 'LaTeX-math-insert-function) (lambda (s) (isearch-process-search-string (gethash (concat "\\" s) julia-mode-latexsubs) (gethash (concat "\\" s) julia-mode-latexsubs))))))

(defun isearch-unsetup-latex-math ()
  (if (in-latexp)
      (set (make-local-variable 'LaTeX-math-insert-function) 'TeX-insert-macro)
    (set (make-local-variable 'LaTeX-math-insert-function) 'julia-math-insert)))

(setq blink-matching-delay 0.2)
(require 'guess-language)
(setq guess-language-languages '(en fr))
(setq guess-language-min-paragraph-length 0)
(defun ispell-guess-language ()
  (interactive ())
  (setq ispell-dictionary (cadr (assq (guess-language-buffer) guess-language-langcodes)))
  (message ispell-dictionary))

(add-hook 'message-mode-hook (lambda () (setq message-signature 'my-signature)))
(define-key message-mode-map (kbd "C-c C-z")  'my-insert-signature)
(define-key message-mode-map (kbd "C-c C-w")  'my-insert-signature)
(defun my-insert-signature ()
  (interactive)
  ;; (insert "\n--\n")
  (insert "\n")
  (insert (my-signature))
  (insert "\n")
  (forward-line -2))

(defun my-signature ()
  (save-excursion
    (let* ((body-point (message-goto-body))
	   (lang (guess-language-region body-point (point-max))))
      (cond ((eq lang 'en) "Best,\nAntoine")
            (t "Amitiés,\nAntoine")))))

(setq guess-language-after-detection-functions nil)

;; (if (string-match "lambda" (shell-command-to-string
;; 			    "hostname"))
;;     (progn
;;       (setq mu4e-headers-fields '((:human-date . 6)
;; 				  (:maildir . 10)
;; 				  (:from-or-to . 22)
;; 				  (:thread-subject . 63)))
;;       (setq sml/name-width 60)))

(require 'electric-operator)
(add-hook 'julia-mode-hook #'electric-operator-mode)
(electric-operator-add-rules-for-mode 'julia-mode
				      (cons "^" nil)
				      (cons ":" nil)
				      (cons "?" nil)
				      (cons "*" nil)
				      (cons "/" nil)
				      (cons "+" nil)
				      (cons "-" nil)
				      (cons "." nil)
				      (cons "->" " -> ")
				      )

(setq preview-pdf-color-adjust-method nil) ;; temp, until my linux upgrades ghostscript
(setq markdown-enable-math t)


(require 'fix-word)
(global-set-key (kbd "M-u") #'fix-word-upcase)
(global-set-key (kbd "M-l") #'fix-word-downcase)
(global-set-key (kbd "M-c") #'fix-word-capitalize)

(require 'ivy-prescient)
(ivy-prescient-mode 1)
(setq prescient-use-char-folding nil) ;; https://github.com/abo-abo/swiper/issues/3017
(prescient-persist-mode 1)

(require 'expand-region)
(global-set-key (kbd "M-h") 'er/expand-region)

(add-hook 'server-visit-hook (lambda ()
			       (local-set-key (kbd "C-c C-c")
					      (lambda ()
						(interactive)
						(save-buffer)
						(server-edit) ; C-x #
						(kill-buffer)))
			       ;; (when (string-match "tmp_emacs_everywhere" (buffer-name))
			       ;; 	 (condition-case err
			       ;; 	     (progn
			       ;; 	       (goto-char (point-min))
			       ;; 	       (search-forward "POINT_HERE")
			       ;; 	       (delete-char -10))
			       ;; 	   (error nil)))
			       ))


;; (quelpa '(eaf :fetcher github
;;               :repo  "manateelazycat/emacs-application-framework"
;;               :files ("*")))

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
;; (require 'eaf)
;; (require 'eaf-pdf-viewer)

;; (eaf-setq eaf-browser-dark-mode nil)
;; (eaf-setq eaf-pdf-dark-mode "false")
;; (setq eaf-pdf-store-history nil)
;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;; (eaf-setq eaf-buffer-background-color (face-background 'default nil))

(setq package-native-compile t)

(defun my-kill-visual-line (&optional arg)
  (interactive "P")
  (if arg
      (cl-letf (((symbol-function 'kill-region)
		 (lambda (beg end &optional yank-handler)
                   (delete-region beg end))))
	(kill-visual-line))
    (kill-visual-line)))
(global-set-key (kbd "C-k") 'my-kill-visual-line)

(defun my-kill-region (&optional arg)
  (interactive "P")
  (if arg
      (call-interactively 'delete-region)
    (call-interactively 'kill-region)))
(global-set-key (kbd "C-w") 'my-kill-region)

(define-key vterm-mode-map (kbd "s-t") 'vterm-copy-mode)
(define-key vterm-mode-map (kbd "C-c C-t") 'vterm-copy-mode)
(define-key vterm-copy-mode-map (kbd "s-t") 'vterm-copy-mode)
(define-key vterm-copy-mode-map (kbd "C-c C-t") 'vterm-copy-mode)
;; (desktop-read)

(define-key mu4e-compose-mode-map (kbd "s-e >")
  (lambda ()
    (interactive)
    (let ((path (concat "/tmp/mu4e/" (cleanse-subject (plist-get mu4e-compose-parent-message ':subject)))))
      (mkdir path t)
      (shell-command-to-string (concat "mu extract --overwrite --target-dir="
				       path
				       " -a \"" (plist-get mu4e-compose-parent-message ':path) "\""))
      (dired path))))
(defun replace-region-by-yank ()
  (interactive)
  (delete-region (point) (mark))
  (yank))
(global-set-key (kbd "C-M-y") 'replace-region-by-yank)

(require 'visual-regexp)
(require 'visual-regexp-steroids)
(global-set-key (kbd "C-M-%") 'vr/query-replace)


;; (setq sml/theme 'respectful)
;; (setq sml/name-width 80)
;; (setq sml/mode-width 30)
;; (setq sml/line-number-format "%4l")
;; (sml/setup)

(require 'spaceline-config)
(require 'spaceline)

(set-face-attribute 'powerline-active2 nil :inherit 'powerline-inactive0 :foreground nil :background nil)
(set-face-attribute 'spaceline-highlight-face nil :inherit 'font-lock-builtin-face :foreground nil :background nil)
(spaceline-spacemacs-theme)
(spaceline-compile
  ; left side
  '(
    persp-name
    (mu4e-alert-segment)
    ;; (mu4e-alert-segment :when active)
    (anzu :priority 95)
    auto-compile
    ((buffer-modified buffer-id remote-host)
     :priority 98)
    (process :when active)
    ((flycheck-error flycheck-warning flycheck-info)
     :when active
     :priority 89)
    (erc-track :when active)
    (org-pomodoro :when active)
    (org-clock :when active)
    nyan-cat)
  ; right side
  '(      (minor-modes :when active
                       :priority 9)
  (major-mode :priority 79)
which-function
    ;; (version-control :when active
    ;;                  :priority 78)
    (python-pyvenv :fallback python-pyenv)
    (purpose :priority 94)
    (battery :when active)
    ;; (selection-info :priority 95)
    input-method
    line
    ;; ((buffer-encoding-abbrev
    ;;   point-position
    ;;   line-column)
    ;;  :separator " | "
    ;;  :priority 96)
    (global :when active)
    ;; (buffer-position :priority 99)
    (hud :priority 99)
))
(setq spaceline-always-show-segments t)
