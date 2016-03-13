setopt EXTENDED_GLOB
setopt PROMPT_SUBST
setopt IGNORE_EOF

function exists {
  command -v "$1" > /dev/null
}

# Use vim as the default editor:
if exists "vim"; then
  export EDITOR=vim
fi

# Store less history in a custom file:
if exists "less"; then
  export LESSHISTFILE=$HOME/.histories/less
fi
