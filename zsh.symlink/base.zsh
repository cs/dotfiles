setopt EXTENDED_GLOB
setopt PROMPT_SUBST
setopt IGNORE_EOF

function exists {
  command -v "$1" > /dev/null
}

# Use nvim as the default editor:
exists "nvim" && export EDITOR=nvim

# Store less history in a custom file:
exists "less"; export LESSHISTFILE=$HOME/.histories/less
