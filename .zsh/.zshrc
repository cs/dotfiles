source "$ZDOTDIR/base.zsh"
source "$ZDOTDIR/prompt.zsh"
source "$ZDOTDIR/path.zsh"
source "$ZDOTDIR/completion.zsh"
source "$ZDOTDIR/aliases.zsh"
source "$ZDOTDIR/history.zsh"

EDITOR=vim # Use vim as the editor
GREP_OPTIONS="--color --line-number" # Make grep more user-friendly

# Use ESC to edit the current command line:
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\033' edit-command-line
