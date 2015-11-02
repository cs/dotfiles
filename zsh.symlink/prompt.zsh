function awesome_prompt() {
  local retval=$?

  # ----------------------------------------------------------------------------
  # Configuration
  # ----------------------------------------------------------------------------

  local directory_separator=''
  local cwd_home_bg=31           # = blueish
  local cwd_home_fg=15           # = white
  local cwd_dir_bg=237           # = dark gray
  local cwd_dir_fg=250           # = almost white
  local cwd_last_dir_bg=237      # = dark gray
  local cwd_last_dir_fg=254      # = white

  local read_only_symbol=''
  local read_only_bg=124         # = red
  local read_only_fg=254         # = white

  local status_symbol='$'
  local status_cmd_passed_bg=235 # = dark gray
  local status_cmd_passed_fg=15  # = almost white
  local status_cmd_failed_bg=161 # = pink
  local status_cmd_failed_fg=15  # = almost white

  # ----------------------------------------------------------------------------
  # Segment Drawing Utilities
  #
  # Depends on `$current_bg` and `$segment_separator`.
  # ----------------------------------------------------------------------------

  local segment_separator=''
  local current_bg='NONE'

  function segment() {
    #
    # Color cheat sheet:
    # %F{red} (%f) -- Set (unset) foreground color to red.
    # %K{red} (%k) -- Set (unset) background color to red.
    # %B (%b)      -- Start (stop) boldface mode.
    # %U (%u)      -- Start (stop) underline mode.
    # %S (%s)      -- Start (stop) standout mode.
    #
    local bg fg
    [[ -n $1 ]] && bg="%K{$1}" || bg="%k"
    [[ -n $2 ]] && fg="%F{$2}" || fg="%f"
    if [[ $current_bg != 'NONE' && $1 != $current_bg ]]; then
      echo -n " %{$bg%F{$current_bg}%}$segment_separator%{$fg%} "
    else
      echo -n "%{$bg%}%{$fg%} "
    fi
    current_bg=$1
    [[ -n $3 ]] && echo -n $3
  }

  # ----------------------------------------------------------------------------
  # Current Working Directory (aka CWD) Segment
  #
  # Depends on `$PWD` and `$HOME`.
  # ----------------------------------------------------------------------------

  local cwd_list="${PWD/#$HOME/~}"        # replace $HOME with '~'
  cwd_list=(${(s:/:)cwd_list})            # split at slashes
  [[ $#cwd_list == 0 ]] && cwd_list=('/') # list empty? => $PWD == '/'

  # Okayyy, list is now ready for rendering...

  # We have a special case, if the first element in the list is a '~'...
  if [[ $cwd_list[1] == '~' ]] ; then
    segment $cwd_home_bg $cwd_home_fg '~'
    cwd_list[1]=() # remove '~' from list
  fi

  # Iterate over all other elements in the list...
  for (( i = 1; i <= $#cwd_list; i++ )) do
    if [[ $i == $#cwd_list ]]; then
      # The last element is special again, because the colors should be slightly
      # different and there's no trailing "separator" symbol needed.
      segment $cwd_last_dir_bg $cwd_last_dir_fg $cwd_list[$i]
    else
      segment $cwd_dir_bg $cwd_dir_fg $cwd_list[$i]
      echo -n " $directory_separator" # render "separator" symbol
    fi
  done

  # ----------------------------------------------------------------------------
  # Read-Only Segment
  #
  # Depends on `$PWD`.
  # ----------------------------------------------------------------------------

  if [[ ! -w $PWD ]]; then
    segment $read_only_bg $read_only_fg $read_only_symbol
  fi

  # ----------------------------------------------------------------------------
  # Git Segment
  #
  # Depends on `git-radar`.
  # See https://github.com/michaeldfallen/git-radar
  # ----------------------------------------------------------------------------

  if exists 'git-radar'; then
    local radar="$(git-radar --zsh)"
    [[ "$radar" != '' ]] && segment black 1 "$radar"
  fi

  # ----------------------------------------------------------------------------
  # Status Segment
  #
  # Depends on `$retval`.
  # ----------------------------------------------------------------------------

  if [[ $retval == 0 ]]; then
    segment $status_cmd_passed_bg $status_cmd_passed_fg $status_symbol
  else
    segment $status_cmd_failed_bg $status_cmd_failed_fg $status_symbol
  fi

  # ----------------------------------------------------------------------------
  # End Segment
  #
  # Depends on `$current_bg` and `$segment_separator`.
  # ----------------------------------------------------------------------------

  if [[ -n $current_bg ]]; then
    echo -n " %{%k%F{$current_bg}%}$segment_separator"
  fi
  echo -n "%{%k%f%}"
}

export PROMPT='%{%k%f%b%}$(awesome_prompt) '
