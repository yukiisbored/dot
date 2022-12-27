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

if [ -d "$HOME/.local/share/coursier" ]; then
    export PATH="$HOME/.local/share/coursier/bin:$PATH"
fi

if [ -d "$HOME/.gnupg/trezor" ]; then
    export GNUPGHOME="$HOME/.gnupg/trezor"
fi

if [ -d "$HOME/.asdf" ]; then
    source "$HOME/.asdf/asdf.sh"
fi

if [ -d "$HOME/flutter" ]; then
    export PATH="$HOME/flutter/bin:$PATH"
fi


if [ -d "$HOME/bin" ]; then
    export PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.opam" ]; then
    source "$HOME/.opam/opam-init/init.zsh"
fi

export GOPATH="$HOME"
export KUBECONFIG="$HOME/.kube/config"

if command -v nvim >/dev/null; then
   export ALTERNATE_EDITOR=""
   export EDITOR="nvim"
   export VISUAL="nvim"
fi

if command -v hx >/dev/null; then
   export ALTERNATE_EDITOR=""
   export EDITOR="hx"
   export VISUAL="hx"
fi
