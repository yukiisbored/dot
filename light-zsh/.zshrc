# Shrinkpath, taken from oh-my-zsh (WTFPL)

shrink_path () {
        setopt localoptions
        setopt rc_quotes null_glob

        typeset -i lastfull=0
        typeset -i short=0
        typeset -i tilde=0
        typeset -i named=0
        typeset -i length=1
        typeset ellipsis=""
        typeset -i quote=0

        if zstyle -t ':prompt:shrink_path' fish; then
                lastfull=1
                short=1
                tilde=1
        fi
        if zstyle -t ':prompt:shrink_path' nameddirs; then
                tilde=1
                named=1
        fi
        zstyle -t ':prompt:shrink_path' last && lastfull=1
        zstyle -t ':prompt:shrink_path' short && short=1
        zstyle -t ':prompt:shrink_path' tilde && tilde=1
        zstyle -t ':prompt:shrink_path' glob && ellipsis='*'
        zstyle -t ':prompt:shrink_path' quote && quote=1

        while [[ $1 == -* ]]; do
                case $1 in
                        --)
                                shift
                                break
                        ;;
                        -f|--fish)
                                lastfull=1
                                short=1
                                tilde=1
                        ;;
                        -h|--help)
                                print 'Usage: shrink_path [-f -l -s -t] [directory]'
                                print ' -f, --fish      fish-simulation, like -l -s -t'
                                print ' -g, --glob      Add asterisk to allow globbing of shrunk path (equivalent to -e "*")'
                                print ' -l, --last      Print the last directory''s full name'
                                print ' -s, --short     Truncate directory names to the number of characters given by -#. Without'
                                print '                 -s, names are truncated without making them ambiguous.'
                                print ' -t, --tilde     Substitute ~ for the home directory'
                                print ' -T, --nameddirs Substitute named directories as well'
                                print ' -#              Truncate each directly to at least this many characters inclusive of the'
                                print '                 ellipsis character(s) (defaulting to 1).'
                                print ' -e SYMBOL       Postfix symbol(s) to indicate that a directory name had been truncated.'
                                print ' -q, --quote     Quote special characters in the shrunk path'
                                print 'The long options can also be set via zstyle, like'
                                print '  zstyle :prompt:shrink_path fish yes'
                                return 0
                        ;;
                        -l|--last) lastfull=1 ;;
                        -s|--short) short=1 ;;
                        -t|--tilde) tilde=1 ;;
                        -T|--nameddirs)
                                tilde=1
                                named=1
                        ;;
                        -[0-9]|-[0-9][0-9])
                                length=${1/-/}
                        ;;
                        -e)
                                shift
                                ellipsis="$1"
                        ;;
                        -g|--glob)
                                ellipsis='*'
                        ;;
                        -q|--quote)
                                quote=1
                        ;;
                esac
                shift
        done

        typeset -i elllen=${#ellipsis}
        typeset -a tree expn
        typeset result part dir=${1-$PWD}
        typeset -i i

        [[ -d $dir ]] || return 0

        if (( named )) {
                for part in ${(k)nameddirs}; {
                        [[ $dir == ${nameddirs[$part]}(/*|) ]] && dir=${dir/#${nameddirs[$part]}/\~$part}
                }
        }
        (( tilde )) && dir=${dir/#$HOME/\~}
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
                        if (( lastfull && $#tree == 1 )) {
                                result+="/$tree"
                                break
                        }
                        expn=(a b)
                        part=''
                        i=0
                        until [[ $i -gt 99 || ( $i -ge $((length - ellen)) || $dir == $part ) && ( (( ${#expn} == 1 )) || $dir = $expn ) ]]; do
                                (( i++ ))
                                part+=$dir[$i]
                                expn=($(echo ${part}*(-/)))
                                (( short )) && [[ $i -ge $((length - ellen)) ]] && break
                        done

                        typeset -i dif=$(( ${#dir} - ${#part} - ellen ))
                        if [[ $dif -gt 0 ]]
                        then
                            (( quote )) && part=${(q)part}
                            part+="$ellipsis"
                        else
                            part="$dir"
                            (( quote )) && part=${(q)part}
                        fi
                        result+="/$part"
                        cd -q $dir
                        shift tree
                }
                echo ${result:-/}
        )
}

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
