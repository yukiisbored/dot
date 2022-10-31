PS1='$(shrink_path -f) %% '

[[ -n "$SSH_TTY" ]] && PS1="$HOST $PS1"
