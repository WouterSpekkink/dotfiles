# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH
path+=/home/wouter/.config/gem/ruby/2.7.0/bin
path+=/home/wouter/.local/bin

autoload -U colors && colors # Load colors

# Path to SSH_AUTH_SOCK
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

# Proper colors
export TERM="xterm-256color"

# Set history file
SAVEHIST=100000000
HISTFILE="$XDG_CACHE_HOME/zsh/zhistory"
setopt share_history

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots) # Include hidden files.

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect '^h' vi-backward-char
bindkey -M menuselect '^k' vi-up-line-or-history
bindkey -M menuselect '^l' vi-forward-char
bindkey -M menuselect '^j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

# Change cursor shape for different vi modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# Scroll history
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# Aliases
alias weechat="weechat -d ${XDG_CONFIG_HOME}/weechat"
alias exult="exult -c $HOME/.local/share/exult/exult.cfg"
alias gephi="$HOME/Tools/gephi-0.10.1/bin/gephi"
alias ls="ls --color"
alias vnc="$HOME/Tools/VNC/VNC-Viewer-6.20.529-Linux-x64 & disown && exit"
alias hdright="xrandr --output HDMI-0 --auto --right-of eDP-1-1"
alias hdleft="xrandr --output HDMI-0 --auto --left-of eDP-1-1"
alias dvright="xrandr --output DVI-I-2-1 --auto --right-of eDP-1-1"
alias dvleft="xrandr --output DVI-I-2-1 --auto --left-of eDP-1-1"
alias dpright="xrandr --output DP-0 --auto --right-of eDP-1-1"
alias dpleft="xrandr --output DP-0 --auto --left-of eDP-1-1"
alias swnv="optimus-manager --switch nvidia"
alias gemsync="rsync -aP ~/Sites/gemini/ gemini:~/gemini/"
alias mail="$HOME/.local/bin/tutanota-desktop-linux.AppImage & disown && exit"
alias trash="trash -i"
alias pioneer="cd $HOME/Games/pioneer && ./pioneer"
alias frontier="cd $HOME/Games/frontier && dosbox frontier.exe -fullscreen"
alias update="sudo pacman -Syu"
alias cleanup='sudo pacman -Rns $(pacman -Qtdq)'
alias update-grub="sudo grub-mkconfig -o /boot/grub/grub.cfg"
alias la="ls -a"
alias yarn="yarn --use-yarnrc $XDG_CONFIG_HOME/yarn/yarnrc"

# NNN configuration
export NNN_FCOLORS='27d0e62e006099f7c6e2abc4'
export NNN_PLUG='u:getplugs;t:treeview;m:nmount;s:suedit;i:imgview;p:preview-tui'
export NNN_COLORS='5421'
export NNN_FIFO='/tmp/nnn.fifo'
export NNN_TRASH=1

# To enable cd on quit

n ()
{
    # Block nesting of nnn in subshells
    if [ n $NNNLVL ] && [ "${NNNLVL:-0}" -ge 1 ]; then
        echo "nnn is already running"
        return
    fi

    # The default behaviour is to cd on quit (nnn checks if NNN_TMPFILE is set)
    # To cd on quit only on ^G, remove the "export" as in:
    #     NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
    # NOTE: NNN_TMPFILE is fixed, should not be modified
    export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef
    # stty lnext undef

    nnn "$@"

    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}

# Run welcome message
welcomemessage

# Load plugins
source $XDG_CONFIG_HOME/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source $XDG_CONFIG_HOME/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
source $XDG_CONFIG_HOME/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Pyenv
eval "$(pyenv init -)"

# Load starthip prompt (should be at the end)
eval "$(starship init zsh)"
