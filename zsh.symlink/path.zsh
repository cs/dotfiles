if [[ -x /usr/local/bin/brew ]]; then
  PATH="/usr/local/sbin:$PATH"
  PATH="/usr/local/bin:$PATH"
fi

PATH="$HOME/.bin:$PATH"
