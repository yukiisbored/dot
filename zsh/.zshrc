# If this is a dumb terminal, drop everything
if [ "$TERM" = "dumb" ]; then
    return
fi

# WSL check
(uname -r | grep -q '[Mm]icrosoft') && WSL=1

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
        (( $+commands[colorls] )) && alias ls="colorls -G"
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
    dir=${dir/#\/mnt\/c\/Users\/Yuki/\~/winhome}
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

[[ -n "$SSH_TTY" ]] && PS1="$HOST $PS1"

# X410
[[ -n "$WSL" ]] && export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0

# Aliases
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

[[ "$INSIDE_EMACS" = 'vterm' ]] && alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'


# Update title
update_title() {
    print -Pn "\e]2;%m:%2~\a"
}

autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd update_title

update_title

# direnv integration
if (( $+commands[direnv] )) {
    eval "$(direnv hook zsh)"
}

# antigen
ZSH_ANTIGEN="$HOME/.antigen.zsh"

function bootstrap_antigen() {
    if [[ ! -f "$ZSH_ANTIGEN" ]] {
        curl -L git.io/antigen -o "$ZSH_ANTIGEN"
    } else {
        echo "antigen already bootstrapped."
    }
}

if [[ -f "$ZSH_ANTIGEN" ]] {
    source "$ZSH_ANTIGEN"

    antigen bundle zsh-users/zsh-history-substring-search
    antigen bundle zsh-users/zsh-autosuggestions
    antigen bundle zsh-users/zsh-completions
    antigen bundle zsh-users/zsh-syntax-highlighting

    antigen apply
}
