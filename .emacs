;;; Emacs of Antoine Levitt. Homepage : http://github.com/antoine-levitt/perso
;; Mainly a mix of many things I found on the net, plus some stuff of mine

;; Can be viewed in outline mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unclutter home directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp files
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/dict"))
;;byte-recompile elisp files if they need to be
(byte-recompile-directory "~/.emacs.d/lisp" 0)

;; gnus/mail directories
(setq gnus-init-file "~/.emacs.d/gnus.el"
      gnus-home-directory "~/.emacs.d"
      mail-default-directory "~/.emacs.d"
      message-directory "~/.emacs.d/Mail")

;; customize
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Desktop and server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;if we are alone, run server, and load desktop
;;very crude hack
(setq emacs-is-master nil)
(when (string= "1\n"
	       (shell-command-to-string
		"ps x | grep emacs | grep -v grep | grep -v emacs-bin | grep -v emacsclient | wc -l"))
  (setq emacs-is-master t)
  (server-start)
  (desktop-save-mode 1))

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
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))
;; one emacs to rule them all and in fullscreen bind them
(when emacs-is-master
  (toggle-fullscreen))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Colour theme and fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'zenburn)
(zenburn)
(setq font-use-system-font t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mouse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;mouse use : paste at point position. Do not highlight
(setq mouse-yank-at-point t
      mouse-highlight 1)
;; control mouse clipboard. In particular, select-active-regions, activated in 23.2, sucks.
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard nil)
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
  (let ((process-connection-type nil))
    (start-process "" nil command filename)))

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

(defun rde () (interactive) (load-file "~/.emacs"))
(defun ede () (interactive) (find-file "~/.emacs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc. settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(defvar backup-dir "~/.emacsbackups/")
(setq backup-directory-alist (list (cons "." backup-dir)))

;;move between windows with meta-arrows
(windmove-default-keybindings 'shift)

;;please add a final newline each time I save a buffer
(setq require-final-newline 't)

(require 'pastebin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;scroll one line at a time
(setq scroll-conservatively 100000000)
;;keep cursor at current position when scrolling
(setq scroll-preserve-screen-position 42)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Silent saves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note that this can not prevent
;; the "Wrote %s" message, which is coded in C.
(defadvice save-buffer (around save-omit-be-quiet activate)
  "Be quiet."
  (flet ((message (&rest args) nil))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Word wrapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; amazing new variable in e23. No need to worry about longlines any more
(setq-default word-wrap t)
;; ... but still use ll sometimes for reading dense text
(defalias 'll 'longlines-mode)

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
;; automatically update buffers when changed
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-interval 30) ;30s is enough
(setq auto-revert-verbose nil)
;; redefine file-remote-p to add a special rule to consider "/net" as remote
(defun file-remote-p (file &optional identification connected)
  ;; might cause false positive. I'll care when I see one
  (if (string-match-p "net/" file)
      t
    (let ((handler (find-file-name-handler file 'file-remote-p)))
      (if handler
	  (funcall handler 'file-remote-p file identification connected)
	nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;makes C-x C-f and C-x b a lot easier
(require 'ido)
(setq ido-create-new-buffer 'always
      ido-enable-flex-matching t
      ido-max-prospects 12
      ido-max-window-height 1
      ido-read-file-name-non-ido '(gnus-mime-save-part))
(ido-mode 1)
(ido-everywhere 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Uniquify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Icomplete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;icomplete : completion for commands that don't use ido (like help)
(icomplete-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parenthesis editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;visual paren matching
(show-paren-mode t)
;;rainbow parentheses highlighting ! \o/
(require 'highlight-parentheses)
(setq hl-paren-colors
      '("red" "orange" "yellow" "green" "light blue" "dark blue" "black"))
(global-highlight-parentheses-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paredit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'paredit)
;;undefine some keys I use for other things
(define-key paredit-mode-map (kbd "M-<down>")
  nil)
(define-key paredit-mode-map (kbd "M-<up>")
  nil)
(define-key paredit-mode-map (kbd "M-\"")
  nil)
(define-key paredit-mode-map (kbd "M-q")
  'paredit-backward-kill-word)
;;automatically run paredit in specific modes
(mapc (lambda (mode)
	(let ((hook (intern (concat (symbol-name mode)
				    "-mode-hook"))))
	  (add-hook hook (lambda () (paredit-mode 1)))))
      '(emacs-lisp lisp inferior-lisp scheme))
(defalias 'par 'paredit-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Autopair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers
(setq autopair-blink nil)
;; not in ERC
(add-hook 'erc-mode-hook
	  #'(lambda () (setq autopair-dont-activate t)))
;; pair $ correctly
(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (modify-syntax-entry ?$ "\"")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc. dired add-ons
(require 'dired-x)
;; omit
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
;;clean dired default view : omit hidden files, don't display groups, use human-readable sizes
(setq dired-listing-switches "-alhG"
      dired-free-space-args "-Pkm"
      dired-auto-revert-buffer t)
;; Omit, be quiet
(defadvice dired-omit-expunge (around dired-omit-be-quiet activate)
  "Be quiet."
  (flet ((message (&rest args) ))
    ad-do-it))
(add-hook 'dired-mode-hook 'dired-omit-mode)

(require 'dired+)
;; copy/pasting in dired
(require 'wuxch-dired-copy-paste)
(define-key dired-mode-map (kbd "M-w") 'wuxch-dired-copy)
(define-key dired-mode-map (kbd "C-w") 'wuxch-dired-cut)
(define-key dired-mode-map (kbd "C-y") 'wuxch-dired-paste)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Psvn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;C-x v s as main svn entry point
;;note : dired customisations have to be done BEFORE this
(require 'psvn)
(global-set-key (kbd "C-x v s") 'svn-examine)
;;default to a clean view.
(setq svn-status-hide-unknown t)
(setq svn-status-hide-unmodified t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Egg for git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'egg)
(setq egg-buffer-hide-help-on-start (quote (egg-status-buffer-mode egg-log-buffer-mode egg-file-log-buffer-mode egg-reflog-buffer-mode egg-diff-buffer-mode egg-commit-buffer-mode))
      egg-buffer-hide-section-type-on-start (quote ((egg-status-buffer-mode . :diff)))
      egg-confirm-next-action nil
      egg-status-buffer-sections '(repo unstaged staged)
      egg-commit-buffer-sections '(staged unstaged))
(defalias 'egg 'egg-status)

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
(setq recentf-max-saved-items nil)
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file-or-maybe-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Imenu: jump between indexes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'imenu)
(defun ido-goto-symbol ()
  "Update the imenu index and then use ido to select a symbol to navigate to."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (push-mark)
      (goto-char position))))
(global-set-key (kbd "C-x C-i") 'ido-goto-symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'al-ibuffer)
;;entry point
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ignore case when matching a suffix (such as .F90)
(setq auto-mode-case-fold t)
;;tags
(setq tags-table-list '("~/.emacs.d" ".")
      tags-revert-without-query t)
;;indent yanked code in programming languages modes
(load-library "yank-indent")
(setq yank-indent-modes '(emacs-lisp-mode
			  c-mode c++-mode
			  tcl-mode sql-mode
			  perl-mode cperl-mode
			  java-mode jde-mode
			  lisp-interaction-mode
			  scheme-mode
			  LaTeX-mode TeX-mode
			  matlab-mode ada-mode))

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
(defun my-latex-environment (arg)
  "Same as `LaTeX-environment', but overrides prefix arg to mean
  insert environment around region. Use C-u C-u to override
  environment (C-u with LaTeX-environment)"
  (interactive "p*")
  (pp arg)
  (if (= arg 4)
      (flet ((TeX-active-mark () t))
	(LaTeX-environment nil))
    (LaTeX-environment (if (= arg 16) t nil))))
(condition-case err
    (progn (load "auctex.el" nil t t)
	   (load "preview-latex.el" nil t t))
  (error
   (message "Failed to load auctex")))
;;don't ask to cache preamble
(setq preview-auto-cache-preamble t)
;;indent when pressing RET
(setq TeX-newline-function 'newline-and-indent
      LaTeX-math-abbrev-prefix (kbd "ù"))
;;always preview using gnome-open
(setq TeX-output-view-style
      '(("pdf" "." "gnome-open %o")
	("dvi" "." "dvipdf %o && gnome-open $(basename %o dvi)pdf")))
(defun my-tex-config ()
  (turn-on-reftex)
  (auto-fill-mode 1)
  (TeX-PDF-mode 1)
  (LaTeX-math-mode 1)
  (local-set-key (kbd "C-c C-d") 'TeX-insert-braces)
  (local-set-key (kbd "C-c l") 'reftex-label)
  (local-set-key (kbd "C-c r") 'reftex-reference)
  (local-set-key (kbd "C-c b") 'reftex-citation)
  (local-set-key (kbd "C-c C-e") 'my-latex-environment)
  ;; undo TeX remaps, otherwise it interferes with compilation
  (define-key TeX-mode-map [remap next-error] nil)
  (define-key TeX-mode-map [remap previous-error] nil)

  ;; If the file contains local variables defining TeX-master, respect that.
  ;; Otherwise, look for a master file in the current directory
  ;; Define a local variable by
  ;; %%% Local Variables:
  ;; %%% TeX-master: "something"
  ;; %%% End:

  ;; list of master files to look for, increasing order of priority
  (setq list-of-master-files '("report" "master" "main"))
  ;; OK, this is a hack, but we force parsing of the file local variables here
  (hack-local-variables)
  (unless (stringp TeX-master)
    (dolist (name list-of-master-files)
      (when (file-exists-p (concat name ".tex"))
  	(setq TeX-master name))))

  ;; setup compilation, based on TeX-master
  ;; needs raise_process, which raises (using wmctrl) a process whose invocation
  ;; line matches the argument
  (let ((master (if (stringp TeX-master)
  		    TeX-master
  		  (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))
    (set (make-local-variable 'compile-command)
  	 (format
  	  "rubber -d %s && (raise_process.sh %s.pdf || nohup gnome-open %s.pdf > /dev/null)"
  	  master master master))))
(add-hook 'LaTeX-mode-hook 'my-tex-config)

(defun my-bibtex-compilation-setup ()
  (set (make-local-variable 'compile-command)
       (format
	"rubber -d main && (raise_process main.pdf || nohup gnome-open main.pdf > /dev/null)")))
(add-hook 'bibtex-mode-hook 'my-bibtex-compilation-setup 'attheend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Outline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq outline-minor-mode-prefix (kbd "s-o"))
(add-hook 'LaTeX-mode-hook 'outline-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(defun setup-outline-lisp ()
  "Only outline on ;;;, thank you."
  (setq outline-regexp ";;; "))
(add-hook 'emacs-lisp-mode-hook 'setup-outline-lisp)
(require 'fold-dwim)
(setq fold-dwim-outline-style 'nested)
;; Have two toggles, one for the header we're in, and one general
(global-set-key (kbd "<f6>")  'fold-dwim-toggle)
(global-set-key (kbd "<f7>")  'fold-dwim-toggle-all)
;; This is suboptimal, not buffer-local, etc. I don't care.
(setq fold-dwim-general-toggle nil)
(defun fold-dwim-toggle-all ()
  (interactive)
  (if fold-dwim-general-toggle
      (fold-dwim-show-all)
    (fold-dwim-hide-all))
  (toggle-variable 'fold-dwim-general-toggle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reftex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'reftex)
(require 'reftex-toc)
(setq reftex-plug-into-AUCTeX t)
(add-hook 'reftex-toc-mode-hook 'delete-other-windows)
;; I need to add a save-window-excursion, or else it'll display the buffer whenever
;; the buffer is reverted
(defun reftex-toc-revert (&rest ignore)
  "Regenerate the *toc* from the internal lists."
  (interactive)
  (save-window-excursion
    (let ((unsplittable
	   (if (fboundp 'frame-property)
	       (frame-property (selected-frame) 'unsplittable)
	     (frame-parameter (selected-frame) 'unsplittable)))
	  (reftex-rebuilding-toc t))
      (if unsplittable
	  (switch-to-buffer
	   (reftex-get-file-buffer-force reftex-last-toc-file))
	(switch-to-buffer
	 (reftex-get-file-buffer-force reftex-last-toc-file))
	)
      )
    (reftex-erase-buffer "*toc*")
    (setq current-prefix-arg nil)
    (reftex-toc t)
    ))
(define-key reftex-toc-map (kbd "q") 'reftex-toc-quit-and-kill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default comint-scroll-to-bottom-on-input 'all
	      comint-move-point-for-output t)
(ansi-color-for-comint-mode-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-startup-indented t)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key (kbd "s-r") 'org-capture)
(global-set-key (kbd "s-a") 'org-agenda-list)

;;bindings
(add-hook 'org-load-hook
	  (lambda ()
	    (define-key org-mode-map (kbd "<C-tab>") nil)
	    (define-key org-mode-map (kbd "C-c C-r") 'org-refile)
	    (define-key org-mode-map (kbd "<S-up>") nil)
	    (define-key org-mode-map (kbd "<S-down>") nil)
	    (define-key org-mode-map (kbd "<S-right>") nil)
	    (define-key org-mode-map (kbd "<S-left>") nil)))

;;settings
(setq
 org-agenda-files (list "~/.emacs.d/org/todo.org")
 org-default-notes-file "~/.emacs.d/org/notes.org"
 org-agenda-ndays 7
 org-log-done 'time
 org-startup-folded 'content
 org-deadline-warning-days 4
 org-agenda-show-all-dates t
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-start-on-weekday 1
 org-agenda-repeating-timestamp-show-all t
 org-reverse-note-order t
 org-capture-templates '(("t" "default" entry
			  (file+headline "~/.emacs.d/org/todo.org" "Tasks")
			  "* TODO %?\nSCHEDULED: %t\n%a\n%i")))

;; French holidays, all from http://www.drieu.org/blog/index.php/APRIL/101029
(defun vacances (string sd sm sy ed em ey)
  "Compute holiday lists"
  (filter-visible-calendar-holidays
   (vacances-i string 
	       (calendar-absolute-from-gregorian (list sm sd sy))
	       (calendar-absolute-from-gregorian (list em ed ey)))))
 
(defun vacances-i (string s e)
  "Holidays iterator"
  (if (= s e)
      nil
    (cons (list (calendar-gregorian-from-absolute s) string)
	  (vacances-i string (+ s 1) e))))
 
(defun feries-paques ()
  "Liste des jours de vacances  relatifs a paques."
  (let* ((century (1+ (/ displayed-year 100)))
	 (shifted-epact	;; Age of moon for April 5...
	  (% (+ 14 (* 11 (% displayed-year 19))	;;     ...by Nicaean rule
		(- ;; ...corrected for the Gregorian century rule
		 (/ (* 3 century) 4))
		(/ ;; ...corrected for Metonic cycle inaccuracy.
		 (+ 5 (* 8 century)) 25)
		(* 30 century))	;;              Keeps value positive.
	     30))
	 (adjusted-epact ;;  Adjust for 29.5 day month.
	  (if (or (= shifted-epact 0)
		  (and (= shifted-epact 1) (< 10 (% displayed-year 19))))
	      (1+ shifted-epact)
	    shifted-epact))
	 (paschal-moon ;; Day after the full moon on or after March 21.
	  (- (calendar-absolute-from-gregorian (list 4 19 displayed-year))
	     adjusted-epact))
	 (abs-easter (calendar-dayname-on-or-before 0 (+ paschal-moon 7)))
	 (day-list
	  (list
	   (list (calendar-gregorian-from-absolute abs-easter)
		 "Pâques")
	   (list (calendar-gregorian-from-absolute (+ abs-easter 1))
		 "Lundi de Pâques")
	   (list (calendar-gregorian-from-absolute (+ abs-easter 39))
		 "Jeudi de l'ascension")
	   (list (calendar-gregorian-from-absolute (+ abs-easter 49))
		 "Pentecôte")
	   (list (calendar-gregorian-from-absolute (+ abs-easter 50))
		 "Lundi de Pentecôte")))
	 (output-list
	  (filter-visible-calendar-holidays day-list)))
    output-list))

(setq calendar-holidays
      '((holiday-fixed 1 1 "Nouvel an")
	(holiday-fixed 5 1 "Fête du travail")
	(holiday-fixed 5 8 "Victoire 1945")
	(feries-paques)
	(holiday-fixed 7 14 "Fête nationale")
	(holiday-fixed 8 15 "Assomption")
	(holiday-fixed 11 11 "Armistice 1918")
	(holiday-fixed 11 1 "Toussaint")
	(holiday-fixed 12 25 "Noël")
	(holiday-float 5 0 2 "Fête des mères")
	(holiday-float 6 0 3 "Fête des pères")
	;; ; à mettre selon les gouts
	;; (vacances "Vacances de la Toussaint" 23 10 2010 4 11 2010)
	;; (vacances "Vacances de Noël" 18 12 2010 3 1 2011)
	;; (vacances "Vacances d'hiver" 12 2 2011 28 2 2011)
	;; (vacances "Vacances de printemps" 9 4 2011 26 4 2011)
	;; (vacances "Vacances d'été" 2 7 2011 5 9 2011)
	))
(setq calendar-mark-holidays-flag t)


(require 'google-weather)
(require 'org-google-weather)
(setq google-weather-unit-system-temperature-assoc '(("SI" . "°C")
						     ("US" . "°F")))

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
 compilation-window-height nil
 compilation-auto-jump-to-first-error t
 compilation-disable-input t)

;;compilation by C-c C-c in modes that don't shadow it
(global-set-key (kbd "C-c C-c") 'compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Style check
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compile-with-style-check ()
  "Use compile's interface for style check, but do not memorise as last compilation command"
  (interactive)
  (let ((cmd compile-command))
    (compile (format "style-check.rb -v %s" buffer-file-name))
    (setq compile-command cmd)))

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
(global-set-key (kbd "C-x 1") 'my-delete-other-windows)

;;make use of that useless ^2 key to do something useful. This can fail on some terminals,
;;so protect
(condition-case err
    (progn
      ;;normal
      (global-set-key (kbd "²") (lambda () (interactive) (insert "\\")))
      ;;isearch
      (define-key isearch-mode-map (kbd "²")
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
;; some packages, e.g. gnus-summary, don't define deletechar but only delete. Fix that by aliasing
(global-set-key (kbd "<deletechar>") (kbd "<delete>"))


;;shortcuts to two-keys commands I often use
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-b") 'switch-to-buffer)
(global-set-key (kbd "s-c") 'compile)
(global-set-key (kbd "s-j") 'compile-with-style-check)
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
(global-set-key (kbd "s-;") 'ede)
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
  (launch-command "gnome-terminal" ""))
(global-set-key (kbd "s-h") 'open-shell-here)
(defun note ()
  (interactive)
  (find-file "~/.emacs.d/org/notes.org"))
(defun todos ()
  (interactive)
  (find-file "~/.emacs.d/org/todo.org"))
(global-set-key (kbd "s-n") 'note)
(global-set-key (kbd "s-t") 'todos)
(global-set-key (kbd "s-l") 'bury-buffer)
;; ghosts of past yanks
(global-set-key (kbd "s-y") (lambda ()
			      (interactive)
			      (popup-menu 'yank-menu)))
(defun duplicate-current-line ()
  (interactive)
  "Duplicate current line"
  (let ((text (buffer-substring (line-beginning-position) (line-end-position))))
    (save-excursion
      (end-of-line)
      (newline)
      (insert text))))
(global-set-key (kbd "s-d") 'duplicate-current-line)

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
(defun switch-to-nth-buffer (n arg)
  "Switches to nth most recent buffer. Ignores erc buffers unless switch-include-erc is non-nil."
  (catch 'tag
    (mapcar (lambda (b)
	      (if (or switch-include-erc
		      (not (eq (buffer-local-value 'major-mode b) 'erc-mode)))
		  (unless (minibufferp b)
					;(unless (string-match "^\\*" (buffer-name b))
		    (if (= n 1)
			(progn
			  (switch-to-buffer b)
			  (throw 'tag nil))
		      (setq n (- n 1))))));)
	    (cdr (buffer-list)))))

(defun switch-to-most-recent-buffer (&optional arg)
  (interactive "P")
  (switch-to-nth-buffer 1 arg))
(defun switch-to-second-most-recent-buffer (&optional arg)
  (interactive "P")
  (switch-to-nth-buffer 2 arg))
(defun switch-to-third-most-recent-buffer (&optional arg)
  (interactive "P")
  (switch-to-nth-buffer 3 arg))

;;fast switching between two buffers
(global-set-key (kbd "<s-tab>") 'switch-to-most-recent-buffer)
(global-set-key (kbd "s-TAB") 'switch-to-most-recent-buffer)
;;fast switching between three buffers
(global-set-key (kbd "<C-tab>") 'switch-to-second-most-recent-buffer)
(global-set-key (kbd "<C-s-tab>") 'switch-to-third-most-recent-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc editing commands without keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun duplicate-region (beg end &optional sep)
  "Duplicate the region"
  (interactive "*r")
  (let ((p (point)))
    (copy-region-as-kill beg end)
    (message "%d" (point))
    (goto-char end)
    (if (stringp sep) (insert sep))
    (yank)
    (goto-char p)))

(defun exchange-lines ()
  "Exchanges line at point with line at mark"
  (interactive)
  (save-excursion
    (transpose-lines 0)))

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

;; isearch ends at the beginning of word
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
  (when (and isearch-forward
	     (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dictionnaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq my-languages '("british" "french"))
(setq my-languages-index 0)
(defun icd ()
  "Cycle between dictionaries"
  (interactive)
  (setq my-languages-index (mod (+ my-languages-index 1) (length my-languages)))
  (setq ispell-dictionary (nth my-languages-index my-languages))
  (message ispell-dictionary))
;;english dictionary, change it with M-x ispell-change-dictionary or M-x icd
(setq ispell-dictionary "british"
      ispell-silently-savep t
      ispell-program-name "aspell")

;; true dictionary : look up words on the internet
(load "dictionary-init")
(global-set-key (kbd "s-w") 'dictionary-search)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; W3M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; w3m
(when (require 'w3m nil t)
  (setq w3m-use-cookies t)
  (setq w3m-use-title-buffer-name t)
  (setq w3m-default-display-inline-images t)
  (setq w3m-toggle-inline-images-permanently nil)
  (setq mm-w3m-safe-url-regexp nil)
  (define-key w3m-minor-mode-map "m"
    'w3m-view-url-with-external-browser)
  (defun w3m-switch ()
    (interactive "")
    (if (eq 'w3m-mode (current-mm))
	(w3m-close-window)
      (w3m)))
  (global-set-key (kbd "s-w") 'w3m-switch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tab completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; need 23.2
(setq tab-always-indent 'complete)
(defun my-dabbrev-expand ()
  (when (and (not (bolp))
	     (looking-at "\\_>"))
    (dabbrev-expand nil)))
(defun my-dabbrev-expand-and-nil ()
  (my-dabbrev-expand)
  nil)
(setq completion-at-point-functions '(my-dabbrev-expand-and-nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notification framework (used in ERC)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;notification
(setq do-not-disturb nil)
;;set this if you don't want to be disturbed by notifications
;;(setq do-not-disturb t)
(require 'xml)
(defun notify (message)
  "Notify user by graphical display"
  (unless do-not-disturb
    (shell-command-to-string (format
			      "gnome-osd-client %s"
			      (shell-quote-argument (concat "" (xml-escape-string
								(if (> (length message) 45)
								    (concat (substring message  0 45) "...")
								  message))))))))

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
      (switch-to-buffer "*Group*")
    (gnus)))
(global-set-key (kbd "s-g") 'run-gnus)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ERC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/erc.el")
;;read personal info (ERC stuff)
(load "~/.emacs.d/priv_emacs.el" t)
