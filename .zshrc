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
    msg "We'll prepare the setup for you ;) (oh-my-zsh, gvm, pyenv, rvm) ..."
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

# Install gvm, if it doesn't exist
if [ ! -d $HOME/.gvm ]; then
    msg "Installing gvm ..."
    bash < <(curl -s -S -L https://raw.githubusercontent.com/moovweb/gvm/master/binscripts/gvm-installer)
fi

# Load gvm
source $HOME/.gvm/scripts/gvm

# Install pyenv
if [ ! -d $HOME/.pyenv ]; then
    msg "Installing pyenv ..."
    git clone https://github.com/yyuu/pyenv.git ~/.pyenv
fi

export PYENV_ROOT=$HOME/.pyenv
eval "$(pyenv init -)"

if [ ! -d $HOME/.rvm ]; then
    msg "Installing rvm ..."
    curl -sSL https://rvm.io/mpapis.asc | gpg2 --import -
    \curl -sSL https://get.rvm.io | bash -s stable
fi

touch ~/.perepared-env

export PATH="$HOME/bin:$PYENV_ROOT/bin:$HOME/.rvm/bin:$PATH"

alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

export EDITOR=vim
