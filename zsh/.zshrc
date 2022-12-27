vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}

export COLORTERM=truecolor

setopt noextendedglob

setopt prompt_subst
PS1='$(shrink_path -f) %% %{$(vterm_prompt_end)%}'
[[ -n "$SSH_TTY" ]] && PS1="$HOST $PS1"

update_title() {
    print -Pn "\e]2;%m:%2~\a"
}

autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd update_title

update_title

FZF_DEFAULT_OPTS="--layout=reverse"
