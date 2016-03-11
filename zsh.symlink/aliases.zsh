alias ls="ls -G --color"
alias ll="ls -Glh --color"
alias la="ls -Glha --color"
alias grep="grep --color --line-number"

if exists "less"; then
  alias less="LESSHISTFILE=$HOME/.histories/less less"
fi

# CLI interface for GitHub installed?
exists "hub" && eval "$(hub alias -s)"

exists "git" && alias g="git"
exists "task" && alias t="task"
exists "pass" && alias p="pass"
