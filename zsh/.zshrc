# Bootstrap zplug

export ZPLUG_HOME="$HOME/.zplug"

if ! command -v git >/dev/null; then
    echo "Git is not installed. Please install git." >&2
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
setopt appendhistory autocd beep extendedglob notify nomatch

bindkey -e

bindkey "^[[1;5D" backward-word
bindkey "^[[1;5C" forward-word
bindkey "^[[3~" delete-char
bindkey "^H" backward-kill-word
bindkey "^[[1;5A" history-substring-search-up
bindkey "^[[1;5B" history-substring-search-down

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

if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    _PS1_HOST='%m '
fi

setopt prompt_subst
PS1='$_PS1_HOST$(shrink_path -f) %% '

# zplug

source "$ZPLUG_HOME/init.zsh"

zplug 'zplug/zplug', hook-build:'zplug --self-manage'
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
