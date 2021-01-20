if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"
    export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
fi

if [ -e "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ]; then
    . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
fi

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

if [ -n "$HOME/.ghcup" ]; then
    export PATH="$HOME/.ghcup/bin:$PATH"
fi

if [ -n "$HOME/.cabal" ]; then
    export PATH="$HOME/.cabal/bin:$PATH"
fi

if [ -d "$HOME/.cargo" ]; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi

export PATH="$HOME/bin:$HOME/.local/bin:$PATH"
export GOPATH="$HOME"

if command -v emacs >/dev/null; then
   export ALTERNATE_EDITOR=""
   export EDITOR="emacs"
   export VISUAL="emacs"
fi
