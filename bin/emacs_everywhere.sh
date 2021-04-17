#!/bin/bash

# Needs xdotool, xclip, wmctrl.
# Inspired by https://github.com/zachcurry/emacs-anywhere, but with different choices
# Needs the following in .emacs:

# (add-hook 'server-visit-hook (lambda ()
# 			       (local-set-key (kbd "C-c C-c")
# 					      (lambda ()
# 						(interactive)
# 						(save-buffer)
# 						(server-edit) ; C-x #
# 						(kill-buffer)))
# 			       (when (string-match "tmp_emacs_everywhere" (buffer-name))
# 				 (condition-case err
# 				     (progn
# 				       (goto-char (point-min))
# 				       (search-forward "POINT_HERE")
# 				       (backward-kill-sexp))
# 				   (error nil)))))

# Then bind this script to a keyboard shortcut.


sleep .15 # doesn't work otherwise for some reason...

EE_WINDOW=$( xdotool getactivewindow )
xdotool type --clearmodifiers --delay 0 POINT_HERE
xdotool key --clearmodifiers Ctrl+a
xdotool key --clearmodifiers Ctrl+c
sleep .1
# work around https://github.com/jordansissel/xdotool/issues/43
xdotool keyup ctrl
xdotool keyup super # I use super for this script

xclip -out -selection clipboard > /tmp/tmp_emacs_everywhere

wmctrl -a "- Emacs" # my emacs window name is like this

emacsclient /tmp/tmp_emacs_everywhere

xdotool windowactivate --sync $EE_WINDOW
sleep .15
xclip -selection clipboard /tmp/tmp_emacs_everywhere
xdotool key --clearmodifiers Ctrl+v
sleep .1
xdotool keyup ctrl
