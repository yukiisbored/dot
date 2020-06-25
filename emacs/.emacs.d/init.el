;; init.el --- Yuki's Emacs setup

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Early things to make emacs better(tm)
(require 'init-early)
(require 'init-defaults)

;; Bootstrap package
(require 'init-package)

;; Load environment variables from shell
(require 'init-shell-path)

;; UI/UX essentials
(require 'init-ui)
(require 'init-prog)

;; Major programming packages
(require 'init-yasnippet)
(require 'init-company)
(require 'init-flycheck)
(require 'init-projectile)

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

;; Org
(require 'init-org)
