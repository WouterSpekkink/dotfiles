# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH
path+=/home/wouter/.config/gem/ruby/2.7.0/bin
path+=/home/wouter/.local/bin

# Path to your oh-my-zsh installation.
export ZSH="/home/wouter/.config/zsh/.oh-my-zsh"

# Path to SSH_AUTH_SOCK
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

# Proper colors
export TERM="xterm-256color"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="af-magic"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git zsh-autosuggestions zsh-syntax-highlighting zsh-history-substring-search)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

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
alias doomsync="$HOME/.config/emacs/bin/doom sync"
alias doomupgrade="$HOME/.config/emacs/bin/doom upgrade"
alias doomdoctor="$HOME/.config/emacs/bin/doom doctor"
alias doompurge="$HOME/.config/emacs/bin/doom purge"
alias doomclean="$HOME/.config/emacs/bin/doom clean"
alias importcal="python $HOME/Tools/exchange2org/__init__.py --calendar ~/org/outlook.org --startday 6 --endday 90"
alias netbeans="netbeans --userdir ${XDG_CONFIG_HOME}/netbeans"
alias qualcoder="python3 $HOME/Tools/QualCoder/qualcoder/qualcoder.py"
alias exult="exult -c $HOME/.local/share/exult/exult.cfg"
alias gephi="$HOME/Tools/gephi-0.9.2/bin/gephi"

# NNN configuration
export NNN_FCOLORS='27d0e62e006099f7c6e2abc4'
export NNN_PLUG='u:getplugs;t:treeview;m:nmount;s:suedit;i:imgview;p:preview-tui'
export NNN_COLORS='5421'
export NNN_FIFO='/tmp/nnn.fifo'

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

# Load starthip prompt (should be at the end)
eval "$(starship init zsh)"
