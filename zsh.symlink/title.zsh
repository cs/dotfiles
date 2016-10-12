# Set the terminal title to current working directory (see $PWD and $HOME).
#
# Examples:
#
#   | pwd       | title |
#   +-----------+-------+
#   | /         | /     |
#   | /foo      | /foo  |
#   | $HOME     | ~     |
#   | $HOME/foo | ~/foo |
#
# Note: precmd is a hook provided by zsh.
function precmd {
  print -Pn "\e]0;${PWD/#$HOME/~}\a"
}

# Set the terminal title to the current command (i.e. the command currently in
#   execution => zsh is waiting for its completion).
#
# Examples:
#
#   | command          | title            |
#   +------------------+------------------+
#   | vim              | vim              |
#   | less foo         | less foo         |
#   | rake a && rake b | rake a && rake b |
#
# Note: preexec is a hook provided by zsh.
function preexec {
  printf "\033]0;%s\a" "$1"
}
