setopt extended_glob
setopt prompt_subst

function exists {
  command -v "$1" > /dev/null
}
