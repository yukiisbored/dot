if [ -n "$HOME/.ghcup" ]; then
    export PATH="$HOME/.ghcup/bin:$PATH"
fi
if [ -d "$HOME/.cargo" ]; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi

if [ -d "$HOME/.gnupg/trezor" ]; then
    export GNUPGHOME="$HOME/.gnupg/trezor"
fi

if [ -d "$HOME/.asdf" ]; then
    source "$HOME/.asdf/asdf.sh"
fi

if [ -d "$HOME/bin" ]; then
    export PATH="$HOME/bin:$PATH"
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
