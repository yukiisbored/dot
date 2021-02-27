# If this is a dumb terminal, drop everything
if [ "$TERM" = "dumb" ]; then
    return
fi

# Essentials
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

setopt share_history appendhistory inc_append_history hist_ignore_space \
       hist_ignore_all_dups hist_find_no_dups hist_reduce_blanks extended_history \
       prompt_subst complete_in_word always_to_end autocd auto_pushd interactive_comments \
       notify long_list_jobs nohup local_options local_traps complete_aliases

bindkey -e

# Colored ls
case "$(uname -s)" in
    OpenBSD)
        if (( $+commands[colorls] ))
        then
            alias ls="colorls -G"
        fi
        ;;
    FreeBSD|DragonFly)
        alias ls="ls -G"
        ;;
    Linux|SunOS)
        alias ls="ls --color"
        ;;
esac

# Prompt
spath() {
    dir=$PWD

    dir=${dir/#$HOME/\~}
    tree=(${(s:/:)dir})
    (
        if [[ $tree[1] == \~* ]] {
            cd -q ${~tree[1]}
            result=$tree[1]
            shift tree
        } else {
            cd -q /
        }

        for dir in $tree; {
            if (( $#tree == 1 )) {
                result+="/$tree"
                break
            }
                expn=(a b)
                part=''
                i=0
                until [[ $i -gt 99 || ( $dir == $part ) && ( (( ${#expn} == 1 )) || $dir = $expn ) ]]; do
                    (( i++ ))
                    part+=$dir[$i]
                    expn=($(echo ${part}*(-/)))
                    [[ $i -ge $length ]] && break
                done

                typeset -i dif=$(( ${#dir} - ${#part} ))
                [[ $dif -le 0 ]] && part="$dir"
                result+="/$part"
                cd -q $dir
                shift tree
        }

        echo ${result:-/}
    )
}

PS1='$(spath -f) %% '

if [[ -n "$SSH_TTY" ]] {
    PS1="$HOST $PS1"
}

# Aliases
if (( $+commands[$EDITOR] )) {
   alias vi="$EDITOR"
   alias vim="$EDITOR"
   alias nano="$EDITOR"
}

if (( $+commands[doas] )) {
   function sudo() {
       echo "It's doas, dummy.">&2
       doas "$@"
   }
} elif (( $+commands[sudo] )) {
   function doas() {
       echo "It's sudo, dummy.">&2
       sudo "$@"
   }
}

# Emacs vterm
vterm_printf(){
    if [[ -n "$TMUX" ]] && ([[ "${TERM%%-*}" = "tmux" ]] || [[ "${TERM%%-*}" = "screen" ]] ) {
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    } elif [[ "${TERM%%-*}" = "screen" ]] {
        printf "\eP\e]%s\007\e\\" "$1"
    } else {
        printf "\e]%s\e\\" "$1"
    }
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

# zinit
ZINIT_DIR="$HOME/.zinit"

function bootstrap_zinit() {
    if [[ ! -d "$HOME/.zinit" ]] {
        mkdir "$ZINIT_DIR"
        git clone https://github.com/zdharma/zinit.git "$ZINIT_DIR/bin"
    } else {
        echo "zinit already bootstrapped."
    }
}

if [[ -d "$HOME/.zinit" ]] {
    source "$HOME/.zinit/bin/zinit.zsh"

    function _yuki_history_substring_search_keybinds() {
        bindkey "^P" history-substring-search-up
        bindkey "^N" history-substring-search-down
    }

    zinit light-mode for \
          zsh-users/zsh-autosuggestions \
          zsh-users/zsh-completions \
          zdharma/fast-syntax-highlighting

    zinit light-mode atload:'_yuki_history_substring_search_keybinds' for \
          zsh-users/zsh-history-substring-search
}
