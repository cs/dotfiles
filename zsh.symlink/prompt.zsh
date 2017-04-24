autoload colors && colors

function awesome_prompt() {
  local retval=$?

  # ----------------------------------------------------------------------------
  # Configuration
  # ----------------------------------------------------------------------------

  local directory_separator=''
  local cwd_home_bg=0           # = blue
  local cwd_home_fg=7           # = pale white
  local cwd_dir_bg=0            # = black
  local cwd_dir_fg=7            # = pale white
  local cwd_last_dir_bg=0       # = black
  local cwd_last_dir_fg=7       # = pale white

  local read_only_symbol=''
  local read_only_bg=1          # = red
  local read_only_fg=7          # = pale white

  local status_symbol='$'
  local status_cmd_passed_bg=4  # = dark gray
  local status_cmd_passed_fg=0  # = black
  local status_cmd_failed_bg=1  # = red
  local status_cmd_failed_fg=0  # = black

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
    # %F{red} (%f) -- Set (unset) foreground color.
    # %K{red} (%k) -- Set (unset) background color.
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
    if [[ $#cwd_list > 0 ]]; then
      echo -n " $directory_separator" # render "separator" symbol
    fi
  fi

  # Iterate over all other elements in the list...
  for (( i = 1; i <= $#cwd_list; i++ )) do
    if [[ $i == $#cwd_list ]]; then
      # The last element is special again, because the colors should be slightly
      # different and there's no trailing "separator" symbol needed.
      segment $cwd_last_dir_bg $cwd_last_dir_fg $cwd_list[$i]
    else
      segment $cwd_dir_bg $cwd_dir_fg ${cwd_list[$i]:0:1}
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
    local radar_fragments=()
    radar_fragments+='%{remote: }'
    radar_fragments+='%{branch}'
    radar_fragments+='%{ :local}'
    radar_fragments+="%{ :stash}"
    radar_fragments+="%{ :changes}"

    local italic_m='%{\x1b[3m%}m%{\x1b[0m%}'
    local master_symbol="%{$reset_color%}$italic_m%{$reset_color%}"

    local radar="$(GIT_RADAR_FORMAT="${(j::)radar_fragments}" \
                   GIT_RADAR_MASTER_SYMBOL="$master_symbol" \
                   git-radar --zsh)"

    [[ "$radar" != '' ]] && segment '8' '' "$radar"
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
