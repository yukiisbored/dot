# Start/Attach tmux if this is an interactive SSH session
if [ -z "$TMUX" ] && [ -n "$SSH_TTY" ] && [[ $- =~ i ]]; then
    if tmux has-session; then
        tmux attach
    else
        tmux
    fi
    exit
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

setopt prompt_subst
PS1='$(shrink_path -f) %% '

# Aliases

alias emacs="$EDITOR"
alias ec="$EDITOR"
alias vi="$EDITOR"
alias vim="$EDITOR"

alias ef="$VISUAL"

if [ -x "/usr/local/sbin/vm" ]; then
    alias vm="sudo /usr/local/sbin/vm"
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

! zplug check --verbose && \
    zplug install

zplug load

# Fortune | Cowsay

if command -v "fortune" >/dev/null && \
        command -v "cowsay" >/dev/null; then
    fortune | cowsay
    echo
fi

# Weather

city="Besançon"

wttr() {
    curl -H "Accept-Language: fr" "wttr.in/$city?F"
}
