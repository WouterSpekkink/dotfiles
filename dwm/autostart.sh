#!/bin/bash

# Autostart some programs
pgrep pulseaudio || pulseaudio --start &
pgrep picom || picom -b --config ~/.config/picom/picom.conf &
pgrep dwmblocks || dwmblocks &
pgrep nitrogen || nitrogen --restore &
pgrep wmname || wmname LG3D &
pgrep emacs || emacs --daemon &
#pgrep nm-applet || nm-applet &
pgrep davmail || davmail &

# Run startup scripts
~/.local/bin/checkaudio &
~/.local/bin/remaps &
