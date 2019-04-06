if [ -d "/usr/games" ]; then
    export PATH="/usr/games:$PATH"
fi

if [ -d "/usr/local/jdk-1.8.0/bin/" ]; then
    export PATH="/usr/local/jdk-1.8.0/bin/:$PATH"
fi

export PATH="$HOME/bin:$PATH"
export GOPATH="$HOME"
