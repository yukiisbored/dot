;; init.el --- Yuki's Emacs setup

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Early things to make emacs better(tm)
(require 'init-early)
(require 'init-defaults)

;; Bootstrap package
(require 'init-package)

;; Load environment variables from shell
(require 'init-shell-path)

;; Overhaul emacs
(require 'init-overhaul)

;; Language server support
(require 'init-eglot)

;; Programming modes
(require 'init-web)
(require 'init-yaml)
(require 'init-rust)
(require 'init-python)
(require 'init-php)
(require 'init-nginx)
(require 'init-markdown)
(require 'init-dockerfile)
(require 'init-lua)
(require 'init-go)
(require 'init-elixir)
(require 'init-java)
(require 'init-haskell)
(require 'init-typescript)
(require 'init-nix)
(require 'init-scala)
(require 'init-clojure)
(require 'init-fsharp)
(require 'init-csharp)

;; Org
(require 'init-org)

;; Mail
(require 'init-mail)

;; IRC
(require 'init-irc)

;; Faces
(require 'init-faces)

;; EXWM
(require 'init-exwm)

(server-start)
