;;; Emacs of Antoine Levitt. Homepage : http://github.com/antoine-levitt/perso
;; Mainly a mix of many things I found on the net, plus some stuff of mine

;; Can be viewed in outline mode

;; customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(horizontal-scroll-bar-mode nil)
 '(inhibit-startup-echo-area-message (user-login-name))
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (jabber flx counsel zenburn-theme undo-tree swiper rainbow-mode rainbow-delimiters pdf-tools paredit-everywhere matlab-mode material-theme magit julia-mode highlight-indentation highlight-indent-guides cython-mode better-defaults autopair auctex)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
;; (setq custom-file "~/.emacs.d/custom.el")
;; (load custom-file)

;; Packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))


(setq myPackages
      '(better-defaults
        material-theme
        auctex
        rainbow-delimiters
        autopair
        julia-mode
        matlab-mode
        paredit
        paredit-everywhere
        cython-mode
        undo-tree
        highlight-indent-guides
        magit
        magic-latex-buffer
        ivy
        counsel
        swiper
        flx))
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

(defun dummy-function (&rest args)nil)
(defun remove-mm-lighter (mm)
  "Remove minor lighter from the mode line."
  (setcar (cdr (assq mm minor-mode-alist)) nil))

;; Better better defaults
(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward)
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward-regexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unclutter home directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp files
                                        ;(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
;;byte-recompile elisp files if they need to be
                                        ;(byte-recompile-directory "~/.emacs.d/lisp" 0)

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

  ;; desktop
  (setq desktop-load-locked-desktop t
	desktop-path '("~/.emacs.d/")
	desktop-dirname "~/.emacs.d/"
	desktop-base-file-name "emacs.desktop")
  (desktop-save-mode 1)
  ;; save every 10mins
  (run-with-timer (* 10 60) (* 10 60) (lambda ()
                                        (cl-letf (((symbol-function 'message) 'dummy-function))
                                          (desktop-save-in-desktop-dir))))
  )
;; greeting message
(add-hook 'after-init-hook (lambda () (message "Welcome back.")) t)
(add-hook 'after-init-hook (lambda () (set-frame-parameter nil 'fullscreen 'fullboth)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graphical display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; there is some stuff in customize, but can't move it
;; here for technical reasons

;; no right fringe
(fringe-mode '(nil . 0))

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
;;mouse use : do not highlight
(setq mouse-highlight 1)
;; control mouse clipboard. In particular, select-active-regions, activated in 23.2, sucks.
(setq select-active-regions nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General-purpose functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun toggle-variable (symb)
  (set symb (not (eval symb))))

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
  (launch-command  "/usr/bin/gnome-open" filename))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc. settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/places")
(require 'saveplace)

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

;;move between windows with meta-arrows
(windmove-default-keybindings 'shift)

;;please add a final newline each time I save a buffer
(setq require-final-newline 't)

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
(setq-default word-wrap t)
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
(setq auto-revert-interval 30) ;30s is enough
(setq auto-revert-verbose nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ido-save-directory-list-file "~/.emacs.d/ido.last")
;;makes C-x C-f and C-x b a lot easier
(require 'ido)
(setq ido-create-new-buffer 'always
      ido-enable-flex-matching t
      ido-max-prospects 20
      ido-max-window-height 1
      ido-read-file-name-non-ido '(gnus-mime-save-part))
;; (ido-mode 1)
;; (ido-everywhere 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Icomplete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;icomplete : completion for commands that don't use ido (like help)
;(icomplete-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Undo-tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'undo-tree)
(setq undo-tree-mode-lighter "")
(global-undo-tree-mode)
(add-hook 'fundamental-mode-hook 'turn-on-undo-tree-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parenthesis editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;visual paren matching
(show-paren-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paredit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'paredit)
;;undefine some keys I use for other things
(define-key paredit-mode-map (kbd "M-<down>") nil)
(define-key paredit-mode-map (kbd "M-<up>") nil)
(define-key paredit-mode-map (kbd "M-\"") nil)
(define-key paredit-mode-map (kbd "M-q") 'paredit-backward-kill-word)
;;automatically run paredit in specific modes
(mapc (lambda (mode)
	(let ((hook (intern (concat (symbol-name mode)
				    "-mode-hook"))))
	  (add-hook hook (lambda () (paredit-mode 1)))))
      '(emacs-lisp lisp inferior-lisp scheme))

;; globally define cool things
(require 'paredit-everywhere)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)
(add-hook 'LaTeX-mode-hook 'paredit-everywhere-mode)
(define-key paredit-everywhere-mode-map (kbd "M-S") 'paredit-splice-sexp)
(define-key paredit-everywhere-mode-map (kbd "C-(") 'paredit-backward-slurp-sexp)
(define-key paredit-everywhere-mode-map (kbd "M-s") nil)

(setq paredit-lighter "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Autopair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers
(setq autopair-blink nil)
;; not in ERC
(add-hook 'erc-mode-hook
	  #'(lambda () (setq autopair-dont-activate t) (autopair-mode -1)))
(add-hook 'term-mode-hook
	  #'(lambda () (setq autopair-dont-activate t) (autopair-mode -1)))
;; pair $ correctly
(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (define-key LaTeX-mode-map (kbd "$") 'self-insert-command)
              ;(modify-syntax-entry ?$ "\"")
              ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc. dired add-ons
(require 'dired-x)
;; omit
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
;;clean dired default view : omit hidden files, don't display groups, use human-readable sizes
(setq dired-listing-switches "-alhGv"
      dired-free-space-args "-Pkm"
      dired-auto-revert-buffer t)
;; Omit, be quiet
(defadvice dired-omit-expunge (around dired-omit-be-quiet activate)
  "Be quiet."
  (cl-letf (((symbol-function 'message) 'dummy-function))
    ad-do-it))
(add-hook 'dired-mode-hook 'dired-omit-mode)

(define-key dired-mode-map (kbd "o") 'dired-find-alternate-file)
(put 'dired-find-alternate-file 'disabled nil)
;;add gnome-open as C-ret
(defun dired-gnome-open-file ()
  "Opens the current file in a Dired buffer."
  (interactive)
  (launch-command "gnome-open" (dired-get-file-for-visit)))
(define-key dired-mode-map (kbd "<C-return>") 'dired-gnome-open-file)
;;add smplayer as M-ret in dired
(defun smplayer-open-file ()
  (interactive)
  (launch-command "smplayer" (dired-get-file-for-visit)))
(define-key dired-mode-map (kbd "M-RET") 'smplayer-open-file)
(define-key dired-mode-map (kbd "²") 'smplayer-open-file)
(define-key dired-mode-map (kbd "œ") 'smplayer-open-file)
(define-key dired-mode-map (kbd "M-o") 'dired-omit-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Winner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;C-c left/right to undo/redo changes in window configuration
(winner-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Recent files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;recent files, interaction with ido
(defun recentf-ido-find-file-or-maybe-list (&optional arg)
  "Find a recent file using Ido, or list all recent files if prefixed"
  (interactive "P")
  (if arg
      (recentf-open-files)
    ;;build alist basename->name, offer user a choice of basenames,
    ;;then get matching file and find it
    (let ((file-alist (mapcar 'basename-cons recentf-list))
	  (basename-list (mapcar 'file-name-nondirectory recentf-list)))
      (let ((file (ido-completing-read
		   "Choose recent file: "
		   (mapcar 'file-name-nondirectory
			   recentf-list)
		   nil t)))
	(when file
	  (find-file (cdr (assoc file file-alist))))))))
(setq recentf-save-file "~/.emacs.d/recentf")
(setq recentf-max-saved-items nil)
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file-or-maybe-list)

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
;;; Latex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load auctex
;; (condition-case err
;;     (progn (load "auctex.el" nil t t)
;; 	   (load "preview-latex.el" nil t t))
;;   (error
;;    (message "Failed to load auctex")))

;;don't ask to cache preamble
(setq preview-auto-cache-preamble t)
(setq preview-preserve-counters t)
;;use synctex for synchronisation with viewer
(setq TeX-source-correlate-method 'synctex)

(require 'magic-latex-buffer)
(add-hook 'LaTeX-mode-hook 'magic-latex-buffer)
(setq magic-latex-enable-suscript nil)
(setq magic-latex-enable-block-align nil)
(setq magic-latex-enable-block-highlight nil)

;;indent when pressing RET
(setq TeX-newline-function 'newline-and-indent
      LaTeX-math-abbrev-prefix (kbd "ù")
      TeX-electric-sub-and-superscript t)
(defun my-tex-config ()
  (turn-on-reftex)
  (auto-fill-mode 1)
  (TeX-PDF-mode 1)
  (TeX-source-correlate-mode)
  (LaTeX-math-mode 1)
  (local-set-key (kbd "C-c C-d") 'TeX-insert-braces)
  (local-set-key (kbd "s-c") 'my-latex-compile)
  (local-set-key (kbd "C-c s") (lambda () (interactive) (reftex-reference "s")))
  (local-set-key (kbd "C-c e") (lambda () (interactive) (reftex-reference "e")))
  (local-set-key (kbd "C-c f") (lambda () (interactive) (reftex-reference "f")))
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-label-alist '(AMSTeX)) ;; eqref
  (setq reftex-ref-macro-prompt nil)
  (fset 'reftex-toc-quit 'reftex-toc-quit-and-kill)
  (set-default 'preview-scale-function 1.6)
  ;; (setq reftex-label-alist nil)

  ;; undo TeX remaps, otherwise it interferes with compilation
  (define-key TeX-mode-map [remap next-error] nil)
  (define-key TeX-mode-map [remap previous-error] nil)

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
  (setq LaTeX-math-list '((?/ "frac" nil nil)
                          (?S "sum" nil nil)))
  (require 'latex)
  (LaTeX-math-initialize))
(add-hook 'LaTeX-mode-hook 'my-tex-config)

(defun my-latex-compile ()
  "Run a special compile for latex files"
  (interactive)
  (setq my-latex-compiling-buffer (current-buffer))
  (compile
   (format
    "rubber -df %s"
    (if (stringp TeX-master)
        TeX-master
      (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))))

(defun my-after-latex-compile (buf stat)
  "Display viewer after compilation"
  (when (and (boundp 'my-latex-compiling-buffer)
	     (equal my-latex-compiling-buffer (window-buffer))
	     (equal stat "finished\n"))
    (with-current-buffer my-latex-compiling-buffer
      (let ((file (if (stringp TeX-master)
		      TeX-master
		    (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))
	(TeX-evince-sync-view)
      ;; put evince to front
        (shell-command-to-string
	 (format "wmctrl -r %s.pdf -t 3 && wmctrl -a %s.pdf"
		 file file)))

      (setq my-latex-compiling-buffer nil))))
(add-hook 'compilation-finish-functions 'my-after-latex-compile)
;; add ~/.tex to the inputs; also in bashrc
(setenv "TEXINPUTS" ":~/.tex")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default comint-scroll-to-bottom-on-input 'all
	      comint-move-point-for-output t
	      comint-input-ring-file-name "~/.emacs.d/comint_history")
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
(add-hook 'org-load-hook
	  (lambda ()
	    (define-key org-mode-map (kbd "C-c C-r") 'org-refile)
	    (define-key org-mode-map (kbd "<C-tab>") nil)
	    (define-key org-mode-map (kbd "<S-up>") nil)
	    (define-key org-mode-map (kbd "<S-down>") nil)
	    (define-key org-mode-map (kbd "<S-right>") nil)
	    (define-key org-mode-map (kbd "<S-left>") nil)))

;;settings
(setq
 org-agenda-files (list "~/.emacs.d/org/todo.org")
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
 org-irc-link-to-logs t)

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
 compilation-auto-jump-to-first-error t
 compilation-disable-input t)

;;compilation by C-c C-c in modes that don't shadow it
(global-set-key (kbd "C-c C-c") 'compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;see http://www.emacswiki.org/emacs/IgnacioKeyboardQuit , with a little bit of modifications
(defun my-keyboard-quit()
  "Escape the minibuffer or cancel region consistently using 'Control-g'.
Normally if the minibuffer is active but we lost focus (say, we clicked away or set the cursor into another buffer)
we can quit by pressing 'ESC' three times. This function handles it more conveniently, as it checks for the condition
of not being in the minibuffer but having it active. Otherwise simply doing the ESC or (keyboard-escape-quit) would
brake whatever split of windows we might have in the frame."
  (interactive)
  (if (not(window-minibuffer-p (selected-window)))
      (if (active-minibuffer-window)
          (keyboard-escape-quit))
    (keyboard-quit)))
(define-key global-map (kbd "C-g") 'my-keyboard-quit)

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
   (message "Failed to bind key to \\. Live with it.")))

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
(global-set-key (kbd "<s-left>") 'winner-undo)
(global-set-key (kbd "<s-right>") 'winner-redo)
(defun open-shell-here ()
  (interactive)
  (launch-command "mate-terminal" ""))
(global-set-key (kbd "s-h") 'open-shell-here)

(defun open-file-explorer-here ()
  (interactive)
  (launch-command "caja" ""))
(global-set-key (kbd "s-j") 'open-file-explorer-here)

(defun note ()
  (interactive)
  (find-file "~/.emacs.d/org/notes.org"))
(defun todos ()
  (interactive)
  (find-file "~/.emacs.d/org/todo.org"))
(global-set-key (kbd "s-n") 'note)
(global-set-key (kbd "s-t") 'todos)
(global-set-key (kbd "s-j") 'journal)
(global-set-key (kbd "s-l") 'bury-buffer)
;; ghosts of past yanks
(global-set-key (kbd "s-y") (lambda ()
			      (interactive)
			      (popup-menu 'yank-menu)))
(global-set-key (kbd "s-m") 'compose-mail)
(defun duplicate-current-line ()
  (interactive)
  "Duplicate current line"
  (let ((text (buffer-substring (line-beginning-position) (line-end-position))))
    (save-excursion
      (end-of-line)
      (newline)
      (insert text))))
(global-set-key (kbd "s-d") 'duplicate-current-line)
;; pretty smileys
(global-set-key (kbd "s-o") (lambda () (interactive) (insert "\\o/")))
(global-set-key (kbd "s-²") (lambda () (interactive) (insert ":-|")))
(global-set-key (kbd "s-œ") (lambda () (interactive) (insert ":-|")))

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
(setq switch-include-erc t)
(defun toggle-switch-to-erc ()
  (interactive)
  (toggle-variable 'switch-include-erc)
  (message "Now %s"
	   (if switch-include-erc "including erc" "excluding erc")))
;;quickly switch buffers
(defun switch-to-nth-buffer (n)
  "Switches to nth most recent buffer. Ignores a bunch of stuff."
  (catch 'tag
    (mapcar (lambda (b)
	      (unless
		  (or
		   (and (not switch-include-erc) (eq (buffer-local-value 'major-mode b) 'erc-mode))
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
;; needs 23.2
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

(defadvice completion-at-point (after completion-at-point-complete-if-failed activate)
  "Fallback on dabbrev if completion didn't do anything useful."
  (unless ad-return-value
    (setq ad-return-value (my-dabbrev-expand))))

(setq completion-show-inline-help nil)
(setq completion-ignore-case t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; System tray
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ERC tray. Needs tray_daemon, http://smeuuh.free.fr/tray_daemon/
;;defined in emacs_perso : list of regexps for which we don't blink
;;the tray icon
(setq erc-tray-inhibit-one-activation nil)
(setq erc-tray-ignored-channels nil)
(setq erc-tray-state nil)
(setq erc-tray-enable t)
(defun erc-tray-change-state-aux (arg)
  "Enables or disable blinking, depending on arg (non-nil or nil)"
  (unless (eq erc-tray-state arg)
    (shell-command-to-string
     (concat "echo " (if arg "B" "b") " > /tmp/tray_daemon_control"))
    (setq erc-tray-state arg)))
(defun erc-tray-change-state (arg)
  "Enables or disable blinking, depending on arg (t or nil).
Additional support for inhibiting one activation (quick hack)"
  (when erc-tray-enable
    (if erc-tray-inhibit-one-activation
	(setq erc-tray-inhibit-one-activation nil)
      (erc-tray-change-state-aux arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gnus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compose mails with message-mode (C-x m)
(setq mail-user-agent 'gnus-user-agent)
;; Run gnus or switch to an existing instance
(defun run-gnus ()
  (interactive)
  (if (get-buffer "*Group*")
      (progn
	(switch-to-buffer "*Group*")
	(gnus-group-get-new-news))
    (gnus)))
(global-set-key (kbd "s-g") 'run-gnus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ERC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(load "~/.emacs.d/erc.el")
;;read personal info (ERC stuff)
;;(load "~/.emacs.d/priv_emacs.el" t)

(setq display-buffer-base-action '(display-buffer-same-window . nil))
;; (setq display-buffer-base-action nil)

(when emacs-is-master
  (set-frame-parameter nil 'fullscreen 'fullboth))

(setenv "LD_LIBRARY_PATH" ".")

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)


(load-theme 'material t) ;; load material theme

(setq visible-bell nil)






























;;; wuxch-dired-copy-paste.el
(require 'dired)
(require 'dired-aux)

(define-key dired-mode-map [(control c)(control c)] 'ignore)
(define-key dired-mode-map [(control c)(control c)] 'wuxch-dired-copy)

(define-key dired-mode-map [(control c)(control x)] 'ignore)
(define-key dired-mode-map [(control c)(control x)] 'wuxch-dired-cut)

(define-key dired-mode-map [(control c)(control v)] 'ignore)
(define-key dired-mode-map [(control c)(control v)] 'wuxch-dired-paste)


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


(define-key dired-mode-map (kbd "M-w") 'wuxch-dired-copy)
(define-key dired-mode-map (kbd "C-w") 'wuxch-dired-cut)
(define-key dired-mode-map (kbd "C-y") 'wuxch-dired-paste)

(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)


(require 'magit)
(global-set-key (kbd "C-x v s") 'magit-status)
(global-set-key (kbd "C-x v p") 'magit-push-implicitly)
(global-set-key (kbd "C-x v f") 'magit-pull-from-upstream)

(setq magit-status-sections-hook
      '(magit-insert-status-headers magit-insert-merge-log magit-insert-rebase-sequence magit-insert-am-sequence magit-insert-sequencer-sequence magit-insert-bisect-output magit-insert-bisect-rest magit-insert-bisect-log magit-insert-unstaged-changes magit-insert-staged-changes magit-insert-stashes magit-insert-unpulled-from-upstream magit-insert-unpulled-from-pushremote magit-insert-unpushed-to-upstream magit-insert-unpushed-to-pushremote))

(magit-auto-revert-mode -1)

(add-hook 'python-mode-hook
          (lambda () (setq forward-sexp-function nil)))


(when (get-buffer "*scratch*") (kill-buffer "*scratch*"))
(setq initial-buffer-choice "~/") 

(ivy-mode 1)
(setq magit-completing-read-function 'ivy-completing-read)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "")
(setq ivy-initial-inputs-alist nil)
(setq uniquify-strip-common-suffix nil)
(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))
(setq ivy-sort-matches-functions-alist '(t))
(setq ivy-virtual-abbreviate 'full)

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "s-f") 'counsel-find-file)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-M-s") 'swiper)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)


(remove-mm-lighter 'ivy-mode)
(remove-mm-lighter 'magic-latex-buffer)
(remove-mm-lighter 'iimage-mode)
(require 'reftex)
(remove-mm-lighter 'reftex-mode)
(remove-mm-lighter 'visual-line-mode)
(remove-mm-lighter 'autopair-mode)
(require 'paredit-everywhere)
(remove-mm-lighter 'paredit-everywhere-mode)
(require 'flx)

(define-key isearch-mode-map (kbd "M-s") 'swiper-from-isearch)

(define-key
  ivy-switch-buffer-map
  (kbd "C-k")
  (lambda ()
    (interactive)
    (kill-buffer ivy--current)
    ;(ivy--reset-state ivy-last)
    ))
