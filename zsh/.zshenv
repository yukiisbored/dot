if [ -d "/usr/games" ]; then
    export PATH="/usr/games:$PATH"
fi

if [ -d "/usr/local/jdk-1.8.0" ]; then
    export JAVA_HOME="/usr/local/jdk-1.8.0"
fi

if [ -d "/usr/local/jdk-11" ]; then
    export JAVA_HOME="/usr/local/jdk-11"
fi

if [ -n "$JAVA_HOME" ]; then
    export PATH="${JAVA_HOME}/bin:$PATH"
fi

if [ -n "$HOME/.composer/vendor" ]; then
    export PATH="$HOME/.composer/vendor/bin:$PATH"
fi

export PATH="$HOME/bin:$HOME/.local/bin:$PATH"
export GOPATH="$HOME"

if command -v emacs >/dev/null; then
   export ALTERNATE_EDITOR=""
   export EDITOR="emacsclient -t"
   export VISUAL="emacsclient -c -a emacs"
fi
