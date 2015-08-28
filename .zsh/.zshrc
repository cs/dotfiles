source $ZDOTDIR/prompt.zsh

# Completion System:
autoload -U compinit
compinit -D

# Shortcuts:
alias ls='ls -G'
alias ll='ls -Glh'
alias la='ls -Glha'
alias g="git"
alias t="task"
alias p="pass"

EDITOR=vim # Use vim as the editor

# Nicer history:
HISTSIZE=100000
SAVEHIST=$HISTSIZE
HISTFILE="$ZDOTDIR/.history"
setopt hist_ignore_space
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt inc_append_history

# Use ESC to edit the current command line:
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\033' edit-command-line
