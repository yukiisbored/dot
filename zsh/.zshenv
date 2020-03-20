if [ -d "/usr/games" ]; then
    export PATH="/usr/games:$PATH"
fi

if [ -d "/usr/local/jdk-1.8.0/bin/" ]; then
    export PATH="/usr/local/jdk-1.8.0/bin/:$PATH"
fi

export PATH="$HOME/bin:$HOME/.local/bin:$PATH"
export GOPATH="$HOME"

if command -v emacs >/dev/null; then
   export ALTERNATE_EDITOR=""
   export EDITOR="emacsclient -t"
   export VISUAL="emacsclient -c -a emacs"
fi
