alias ls='ls -G'
alias ll='ls -Glh'
alias la='ls -Glha'

export EDITOR=vim # Use vim as the editor

# Nicer history:
export HISTSIZE=100000
export SAVEHIST=$HISTSIZE

# Use ESC to edit the current command line:
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\033' edit-command-line
