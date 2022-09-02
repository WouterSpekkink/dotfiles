#!/bin/bash

# Autostart some programs
pgrep picom || picom -b --config ~/.config/picom/picom.conf &
pgrep dwmblocks || dwmblocks &
pgrep nitrogen || nitrogen --restore &
pgrep xscreensaver || xscreensaver -nosplash &
pgrep wmname || wmname LG3D &
pgrep emacs || emacs --daemon &
pgrep nm-applet || nm-applet &
pgrep davmail || davmail &

# Run startup scripts
~/.local/bin/checkaudio &
~/.local/bin/remaps &
