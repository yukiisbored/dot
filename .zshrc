# Yuki's .zshrc

function msg {
    echo -e "\033[1;34m>> \033[0m$@"
}

if ! which curl > /dev/null 2>&1; then
    msg "It looks like curl is not installed, please install curl."
    return
fi

if ! which git > /dev/null 2>&1; then
    msg "It looks like git is not installed, please install git."
    return
fi

if [ ! -f $HOME/.prepared-env ]; then
    msg "Hello $(whoami) o/"
    touch ~/.prepared-env
fi

export ZSH=$HOME/.oh-my-zsh

# Install oh-my-zsh, if doesn't exist
if [ ! -d $ZSH ]; then
    msg "Installing oh-my-zsh ..."
    git clone https://github.com/robbyrussell/oh-my-zsh $ZSH
fi

# oh-my-zsh configuration
ZSH_THEME="gentoo"
plugins=(git)

# load oh-my-zsh
source $ZSH/oh-my-zsh.sh

alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

export EDITOR=vim
