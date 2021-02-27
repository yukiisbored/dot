if [ "$TERM" = "dumb" ]; then
    return
fi

# Check if we're in WSL
[ -f /proc/version ] &&
    WSL=$(grep -i 'microsoft' /proc/version)

if [ -n "$WSL" ]; then
    # Load system-wide profile
    source /etc/profile
fi

# Bootstrap zplug
export ZPLUG_HOME="$HOME/.zplug"

if ! command -v git >/dev/null; then
    echo "Git is not installed. Please install git." >&2
    return
fi

if ! command -v curl >/dev/null; then
    echo "cURL is not installed. Please install curl." >&2
    return
fi

if [ ! -d "$ZPLUG_HOME" ]; then
    echo "Installing zplug ..." >&2
    git clone https://github.com/zplug/zplug "$ZPLUG_HOME"
fi

# Tell GPG which tty to use

export GPG_TTY="$(tty)"

# Zsh config

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

setopt share_history appendhistory inc_append_history hist_ignore_space \
       hist_ignore_all_dups hist_find_no_dups hist_reduce_blanks extended_history \
       prompt_subst complete_in_word always_to_end autocd auto_pushd interactive_comments \
       notify long_list_jobs nohup local_options local_traps complete_aliases

bindkey -e

# Use history substring instead of normal history browsing
bindkey "^P" history-substring-search-up
bindkey "^N" history-substring-search-down

case "$(uname -s)" in
    "OpenBSD")
        if command -v colorls >/dev/null; then
            alias ls="colorls -G"
        fi
        ;;
    "FreeBSD")
        alias ls="ls -G"
        ;;
    "DragonFly")
        alias ls="ls -G"
        ;;
    "Linux")
        alias ls="ls --color"
        ;;
    "SunOS")
        # TODO: Proper checks to see if it's GNU
        alias ls="ls --color"
        ;;
    *)
        # Boo, no colored ls :(
        ;;
esac

# Prompt

case "$TERM" in
    "dumb")
        PS1="% "
        ;;
    xterm*|rxvt*|eterm*|screen*)
        PS1='$(shrink_path -f) %% '

        if [ -n "$SSH_TTY" ]; then
            PS1="$HOST $PS1"
        fi
        ;;
esac

# Aliases

if command -v "emacs" >/dev/null; then
   alias vi="$EDITOR"
   alias vim="$EDITOR"
fi

if command -v "doas" >/dev/null; then
   function sudo() {
       echo "It's doas, dummy.">&2
       doas "$@"
   }
elif command -v "sudo" >/dev/null; then
   function doas() {
       echo "It's sudo, dummy.">&2
       sudo "$@"
   }
fi

# zplug

source "$ZPLUG_HOME/init.zsh"

zplug "zplug/zplug", hook-build:"zplug --self-manage"
zplug "plugins/python", from:oh-my-zsh
zplug "plugins/pip", from:oh-my-zsh
zplug "plugins/virtualenvwrapper", from:oh-my-zsh
zplug "plugins/shrink-path", from:oh-my-zsh

zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-autosuggestions"
zplug "zdharma/fast-syntax-highlighting"
zplug "zsh-users/zsh-completions"
zplug "lukechilds/zsh-nvm"

! zplug check && \
    zplug install

zplug load

# Fortune | Cowsay

if command -v "fortune" >/dev/null && \
        command -v "cowsay" >/dev/null; then
    fortune | cowsay
    echo
fi

# Weather

city="Belfort"

wttr() {
    curl -H "Accept-Language: fr" "wttr.in/$city?F"
}

# Emacs vterm

vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}

vterm_prompt_end() {
    vterm_printf "51;A$USER@$HOST:$(pwd)";
}

PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
