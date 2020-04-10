set fish_greeting # Disable greeting when launching a new terminal

fish_vi_key_bindings

command -v git > /dev/null && abbr --add --global g git
command -v moment > /dev/null && abbr --add --global m moment
command -v pass > /dev/null && abbr --add --global p pass

if command -v nvim > /dev/null
  abbr --add --global vi nvim
  abbr --add --global vim nvim
  set --export EDITOR nvim
else if command -v vim > /dev/null
  abbr --add --global vi vim
  abbr --add --global nvim vim
  set --export EDITOR vim
else
  abbr --add --global vim vi
  abbr --add --global nvim vi
  set --export EDITOR vi
end

if command -v fzf > /dev/null
  set --export FZF_CTRL_T_COMMAND "fd --type f --hidden"
  set --export FZF_DEFAULT_OPTS "--bind 'ctrl-a:select-all,ctrl-d:deselect-all'"
  fzf_key_bindings
end

if command -v hledger > /dev/null
  set --export LEDGER_FILE "$HOME/schiessl-it-consulting/ACCOUNTING.journal"
end
