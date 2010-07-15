;; -*- coding: utf-8 -*-
;; Emacs of Antoine Levitt. Homepage : http://smeuuh.free.fr
;; Mainly a mix of many things I found on the net. I coded all the big
;; functions myself though : most ERC stuff, tab completion, recent
;; files and compilation.

;;library path : used for require, load-library, autoload ...
(add-to-list 'load-path (expand-file-name "~/.elfiles"))
(add-to-list 'load-path (expand-file-name "~/.elfiles/dict"))

;;byte-recompile elisp files if they need to be
(byte-recompile-directory "~/.elfiles" 0)

;;desktop and server
;;if we are alone, run server, and load desktop
;;very crude hack
(setq emacs-is-master nil)
(when (string= "1\n"
	       (shell-command-to-string
		"ps x | grep emacs | grep -v grep | grep -v emacs-bin | grep -v emacsclient | wc -l"))
  (setq emacs-is-master t)
  (server-start)
  (desktop-save-mode 1))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diredp-date-time ((t (:foreground "#8fb28f"))))
 '(diredp-dir-heading ((t (:foreground "LightBlue"))))
 '(diredp-dir-priv ((t (:foreground "LightBlue"))))
 '(diredp-symlink ((t (:foreground "Grey"))))
 '(diredp-exec-priv ((t nil)))
 '(diredp-file-name ((t (:foreground "White"))))
 '(diredp-file-suffix ((t (:foreground "Grey"))))
 '(diredp-flag-mark ((t (:foreground "Yellow"))))
 '(diredp-flag-mark-line ((t (:foreground "red"))))
 '(diredp-inode+size ((t (:foreground "LightBlue"))))
 '(diredp-no-priv ((t nil)))
 '(diredp-other-priv ((t nil)))
 '(diredp-link-priv ((t (:foreground "Grey"))))
 '(diredp-rare-priv ((t (:foreground "Magenta"))))
 '(diredp-read-priv ((t nil)))
 '(diredp-write-priv ((t nil))))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-echo-area-message (user-login-name))
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

(require 'zenburn)
(zenburn)
(setq font-use-system-font t)


;; general use functions
(defun toggle-variable (symb)
  (set symb (not (eval symb))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Minor modes, useful stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;display time
(setq display-time-default-load-average nil)
(setq display-time-24hr-format t)
(display-time-mode 1)
(defun insert-time ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %R")))
;;ido : makes C-x C-f and C-x b a lot easier
(require 'ido)
(setq ido-create-new-buffer 'always
      ido-enable-flex-matching t
      ido-max-prospects 12
      ido-max-window-height 1)
(ido-mode 1)
(ido-everywhere 1)

;; uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; amazing new variable in e23. No need to worry about longlines any more
(setq-default word-wrap t)
;; ... but still use ll sometimes for reading dense text
(defalias 'll 'longlines-mode)
;; no right fringe
(fringe-mode '(nil . 0))

;;icomplete : completion for commands that don't use ido (like help)
(icomplete-mode 1)

;;paren stuff
;;visual paren matching
(show-paren-mode t)
;;rainbow parentheses highlighting ! \o/
(require 'highlight-parentheses)
;;highlight-parentheses is a buffer-local minor mode : create a global
;;minor mode of our own
(define-globalized-minor-mode hl-paren-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(setq hl-paren-colors
      '("red" "orange" "yellow" "green" "light blue" "dark blue" "black"))
(hl-paren-mode t)


;;paredit
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)
;;undefine keys I use
(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-<down>")
       nil)
     (define-key paredit-mode-map (kbd "M-<up>")
       nil)
     (define-key paredit-mode-map (kbd "M-\"")
       nil)
     (define-key paredit-mode-map (kbd "M-q")
       'paredit-backward-kill-word)))
;;paredit in specific modes
(mapc (lambda (mode)
	(let ((hook (intern (concat (symbol-name mode)
				    "-mode-hook"))))
	  (add-hook hook (lambda () (paredit-mode 1)))))
      '(emacs-lisp lisp inferior-lisp scheme))
;;toggle paredit with f6
(global-set-key (kbd "<f6>") 'paredit-mode)

;;autopair
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers
(setq autopair-blink nil)
(add-hook 'erc-mode-hook
	  #'(lambda () (setq autopair-dont-activate t)))
(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (modify-syntax-entry ?$ "\"")))

;;indent yanked code in programming languages modes
(load-library "yank-indent")

;;dired
(require 'dired-x)
(require 'dired+)
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
;;add smplayer as M-ret
(defun smplayer-open-file ()
  (interactive)
  (launch-command "smplayer" (dired-get-file-for-visit)))
(define-key dired-mode-map (kbd "M-RET") 'smplayer-open-file)

(defun launch-command (command filename)
  "Launches command with argument filename, discarding all output"
  (let ((process-connection-type nil))
    (start-process "" nil command filename)))

(defun gnome-open-file (filename)
  "gnome-opens the specified file."
  (interactive "fFile to open: ")
  (let ((process-connection-type nil))
    (start-process "" nil "/usr/bin/gnome-open" filename)))

(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
;;clean dired default view : omit hidden files, don't display groups, use human-readable sizes
(setq dired-listing-switches "-alhG"
      dired-free-space-args "-Pkm"
      dired-auto-revert-buffer t)

(add-hook 'dired-mode-hook 'dired-omit-mode)

;;C-x v s as main svn entry point
;;note : dired customisations have to be done BEFORE this
(require 'psvn)
(global-set-key (kbd "C-x v s") 'svn-examine)
;;default to a clean view.
(setq svn-status-hide-unknown t)
(setq svn-status-hide-unmodified t)

(require 'egg)
(setq egg-buffer-hide-help-on-start (quote (egg-status-buffer-mode egg-log-buffer-mode egg-file-log-buffer-mode egg-reflog-buffer-mode egg-diff-buffer-mode egg-commit-buffer-mode))
      egg-buffer-hide-section-type-on-start (quote ((egg-status-buffer-mode . :diff)))
      egg-confirm-next-action nil
      egg-status-buffer-sections '(repo unstaged staged)
      egg-commit-buffer-sections '(staged unstaged))


;;indentation automatique avec entrée
(global-set-key (kbd "RET") 'newline-and-indent)

;;default to no transient mark
(transient-mark-mode -1)

;;C-c left/right to undo/redo changes in window configuration
(winner-mode 1)

;;recent files
(defun basename-cons(f)
  (cons (file-name-nondirectory f) f))
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

;;edit a file as root
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(setq recentf-max-saved-items nil)
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file-or-maybe-list)

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
;;ERC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs_erc.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;entry point
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;never show emacs usual buffers
(setq ibuffer-never-show-predicates
      '("\\*scratch\\*"
	"\\*Messages\\*"
	"6667"))
;;separate buffers in groups
(defun current-mm ()
  (buffer-local-value 'major-mode (current-buffer)))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;language-specific major modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ignore case when matching a suffix (such as .F90)
(setq auto-mode-case-fold t)
;;matlab
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)

(setq matlab-indent-function t)
(setq matlab-verify-on-save-flag nil)
(setq matlab-auto-fill nil)
(setq matlab-fill-code nil)
(setq matlab-shell-command-switches '("-nojvm"))
(add-hook 'matlab-mode-hook (lambda ()
			      (local-set-key (kbd "M-q") 'backward-kill-word)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;major mode customisations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;tags
(setq tags-table-list '("~/.emacs.d")
      tags-revert-without-query t)
;;C mode
;;linux style
(setq c-default-style "linux")
;;'electric' indentation : indent on newline
(add-hook 'c-mode-common-hook (lambda ()
				(define-key c-mode-base-map "\C-m"
				  'c-context-line-break)))

;;Latex mode
(condition-case err
    (progn (load "auctex.el" nil t t)
	   (load "preview-latex.el" nil t t))
  (error
   (message "Failed to load auctex")))
;;reftex
(require 'reftex)
(require 'reftex-toc)
(setq reftex-plug-into-AUCTeX t)
(define-key reftex-toc-map (kbd "q") 'reftex-toc-quit-and-kill)
;;don't ask to cache preamble
(setq preview-auto-cache-preamble t)
;;indent when pressing RET
(setq TeX-newline-function 'newline-and-indent
      LaTeX-math-abbrev-prefix (kbd "ù"))
;;always preview using gnome-open
(setq TeX-output-view-style
      '(
	("pdf" "." "gnome-open %o")
	("dvi" "." "dvipdf %o && gnome-open $(basename %o dvi)pdf")
	))
(defun my-tex-config ()
  (turn-on-reftex)
  (auto-fill-mode 1)
  (TeX-PDF-mode 1)
  (LaTeX-math-mode 1)
  (local-set-key (kbd "C-c C-d") 'TeX-insert-braces)
  (local-set-key (kbd "C-c l") 'reftex-label)
  (local-set-key (kbd "C-c r") 'reftex-reference)
  (local-set-key (kbd "C-c b") 'reftex-citation)
					; if a main.tex exists, assume it is a master file
  (setq TeX-master (if (file-exists-p "main.tex")
		       "main"
		     t)))
(add-hook 'LaTeX-mode-hook 'my-tex-config)

;;shell
(setq-default comint-scroll-to-bottom-on-input 'all
	      comint-move-point-for-output t)
(ansi-color-for-comint-mode-on)


;;org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-startup-indented t)
(require 'remember)
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key (kbd "s-r") 'remember)
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

					;settings
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
 org-remember-store-without-prompt t
 org-remember-templates (quote ((116 "* TODO %?" "~/.emacs.d/org/todo.org" "Tasks")
				(110 "* %?" "~/.emacs.d/org/notes.org" "Notes")))
 remember-annotation-functions (quote (org-remember-annotation))
 remember-handler-functions (quote (org-remember-handler)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;make compile window disappear after successful compilation
(setq compilation-finish-function
      (lambda (buf str)
	(if (string-match "*Compilation*" (buffer-name buf))
	    (if (string-match "abnormally" str)
		(message "There were errors :-(")
	      ;;no errors, make the compilation window go away in 1 second
	      (run-at-time 1 nil
			   (lambda (buf)
			     (delete-windows-on buf)
			     (bury-buffer buf))
			   buf)
	      (message "No errors :-)")))))

;;my-compile is smarter about how to display the new buffer
(defun display-buffer-by-splitting-largest (buffer force-other-window)
  "Display buffer BUFFER by splitting the largest buffer vertically, except if
  there is already a window for it."
  (or (get-buffer-window buffer)
      (let ((new-win
	     (with-selected-window (get-largest-window)
	       (split-window-vertically))))
	(set-window-buffer new-win buffer)
	new-win)))

(defun my-compile ()
  "Ad-hoc display of compilation buffer."
  (interactive)
  (let ((display-buffer-function 'display-buffer-by-splitting-largest))
    (call-interactively 'compile)))

;;misc compilation settings
(setq-default
 compile-command "make"
 compilation-read-command nil
 compilation-scroll-output 'first-error
 compilation-ask-about-save nil
 compilation-window-height 6
 compilation-auto-jump-to-first-error t
 compilation-disable-input t)

;;compilation by C-c C-c in modes that don't shadow it
;;(else s-c)
(global-set-key (kbd "C-c C-c") 'my-compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;keybindings
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
;;for it like ido does
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
;;shortcuts to region commenting
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c c") 'comment-region)
;;quite handy
(defun my-kill-whole-line ()
  (interactive)
  (let ((col (current-column)))
    (kill-whole-line 1)
    (move-to-column col)))
(global-set-key (kbd "C-S-k") 'my-kill-whole-line)
;;sometimes useful (for query-replace and such)
(global-set-key (kbd "C-c C-SPC") 'transient-mark-mode)
;;no keybinding for these two, my map is full :-(
(defun rde () (interactive) (load-file "~/.emacs"))
(defun ede () (interactive) (find-file "~/.emacs"))
;;easy window management for azerty users
(global-set-key (kbd "M-é") 'split-window-vertically)
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
(defun current-buffer-not-mini ()
  "Return current-buffer if current buffer is not the *mini-buffer*
  else return buffer before minibuf is activated."
  (if (not (window-minibuffer-p)) (current-buffer)
    (if (eq (get-lru-window) (next-window))
	(window-buffer (previous-window)) (window-buffer (next-window)))))
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

;;zap to isearch
(defun zap-to-isearch ()
  (interactive)
  (kill-region isearch-opoint isearch-other-end)
  (isearch-done)
  (if (> isearch-other-end isearch-opoint)
      (backward-word)
    (forward-word)))

(define-key isearch-mode-map (kbd "M-z") 'zap-to-isearch)


;;bindings starting with super
(global-set-key (kbd "C-c o") 'ff-find-other-file)

;;shortcuts to two-keys commands I often use
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-b") 'switch-to-buffer)
(global-set-key (kbd "s-c") 'my-compile)
(global-set-key (kbd "s-f") 'find-file)
(global-set-key (kbd "s-u") 'undo)
(global-set-key (kbd "s-i") 'iwb)
(global-set-key (kbd "s-x") 'exchange-point-and-mark)
(global-set-key (kbd "s-SPC") 'pop-global-mark)
(global-set-key (kbd "s-;") 'ede)
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


(setq switch-include-erc t)
(defun toggle-switch-to-erc ()
  (interactive)
  (toggle-variable 'switch-include-erc)
  (message "Now %s"
	   (if switch-include-erc "including erc" "excluding erc")))
(global-set-key (kbd "<f7>") 'toggle-switch-to-erc)
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

;;fast switching between two buffers
(global-set-key [\s-tab] 'switch-to-most-recent-buffer)
;;fast switching between three buffers
(global-set-key (kbd "<C-tab>") 'switch-to-second-most-recent-buffer)


;; multifunction
(defun matlab-go ()
  (interactive)
  (switch-to-buffer (concat "*" matlab-shell-buffer-name "*")))

(setq mf-command-alist
      '(
	(matlab-mode . matlab-go)
	(c-mode . my-compile)
	(erc-mode . irc-dwim)
	))
(defun mf-do ()
  "Execute an action for this major mode. See mf-command-alist"
  (interactive)
  (let ((cmd (assq (current-mm) mf-command-alist)))
    (if cmd
	(call-interactively (cdr cmd))
      (message "No action defined for this mode"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fix behavior of quit-window
(defadvice quit-window (around back-to-one-window)
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
(ad-activate 'quit-window)

;;misc functions
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

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
(global-set-key (kbd "C-c C-k") 'kill-whitespace)

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




;;setup term mode : f5 to switch in/out of term, f6 to switch
;;line/char mode (enables other keybindings in term buffer)
(defun my-term-change-mode ()
  "Change term mode : char-mode <-> line-mode"
  (interactive)
  (if (string= mode-name "Term")
      (if (term-in-char-mode)
	  (term-line-mode)
	(term-char-mode))
    (message "Sorry, buffer is not in Term mode.")))
(global-set-key [f5] (lambda ()
		       (interactive)
		       (with-current-buffer
			   (term "/bin/bash")
			 (term-line-mode))))
(add-hook 'term-mode-hook
	  (lambda ()
	    ;;line mode
	    (local-set-key [f5] 'bury-buffer)
	    (local-set-key [f6] 'my-term-change-mode)
	    ;;char mode
	    (define-key term-raw-map [f5] 'bury-buffer)
	    (define-key term-raw-map [f6] 'my-term-change-mode)))

;;scrolling
;;scroll one line at a time
(setq scroll-conservatively 100000000)
;;keep cursor at current position when scrolling
(setq scroll-preserve-screen-position 42)

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

;;ediff
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

;;isearch gadgets
;;C-o in isearch brings up every hit
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
	       (regexp-quote isearch-string))))))

;;english dictionary, change it with M-x ispell-change-dictionary
(setq ispell-dictionary "british"
      ispell-silently-savep t
      ispell-program-name "aspell")

;; true dictionary : look up words
(load "dictionary-init")
;;(global-set-key (kbd "s-w") 'dictionary-search)


;; w3m
(setq w3m-use-cookies t)
(setq w3m-use-title-buffer-name t)
(setq w3m-display-inline-images t)
(defun w3m-switch ()
  (interactive "")
  (if (eq 'w3m-mode (current-mm))
      (w3m-close-window)
    (w3m)))
(global-set-key (kbd "s-w") 'w3m-switch)

;;mouse use : paste at point position
(setq mouse-yank-at-point t)

;;make tab the ultimate completion key
(defmacro ad-add-advice-to-key (key expr)
  "Around advice the key KEY with expression EXPR. KEY should be
a key in the format accepted by key-binding and such, and EXPR an
expression of the same type as those required by around advices"
  `(add-hook 'pre-command-hook
	     (lambda ()
	       (when (equal (this-command-keys-vector) ,key)
		 (ad-add-advice this-command
				'(azerrswdf ;arbitrary advice name
				  nil	    ;not protected
				  t	    ;activated
				  (lambda ()
				    ,expr
				    (ad-unadvise this-command)))
				'around
				'last)
		 (ad-activate this-command)))))

(ad-add-advice-to-key [9]
		      (let ((p (point)))
			ad-do-it
			(when (and (not (minibuffer-window-active-p (minibuffer-window)))
				   (not (eq (current-mm) 'term-mode))
				   (= p (point))
				   (not (bolp))
				   (looking-at "\\_>"))
			  (dabbrev-expand nil))))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;read personal info (ERC stuff)
(load "~/.emacs_perso.el" t)

;; (global-set-key (kbd "<down-mouse-1>") (lambda () (interactive) (message "non")))
;; (global-set-key (kbd "<mouse-1>") (lambda () (interactive) (message "non")))
;; (global-set-key (kbd "<drag-mouse-1>") (lambda () (interactive) (message "non")))
;; (global-set-key (kbd "<down-mouse-3>") (lambda () (interactive) (message "non")))
;; (global-set-key (kbd "<mouse-3>") (lambda () (interactive) (message "non")))
;; (global-set-key (kbd "<drag-mouse-3>") (lambda () (interactive) (message "non")))

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


;; gnus
(global-set-key (kbd "s-g") 'gnus)
;; compose mails with message-mode (C-x m)
(setq mail-user-agent 'gnus-user-agent)
