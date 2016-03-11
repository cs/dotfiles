setopt EXTENDED_GLOB
setopt PROMPT_SUBST
setopt IGNORE_EOF

function exists {
  command -v "$1" > /dev/null
}
