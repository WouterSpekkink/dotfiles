#!/bin/bash

# Autostart some programs
pgrep picom || picom -b --config ~/.config/picom/picom.conf &
pgrep dwmblocks || dwmblocks &
pgrep nitrogen || nitrogen --restore &
pgrep wmname || wmname LG3D &
pgrep emacs || emacs --daemon &
pgrep davmail || davmail &
pgrep pulseaudio || pulseaudio --start --daemonize &

# Run startup scripts
~/.local/bin/checkaudio &
~/.local/bin/remaps &
