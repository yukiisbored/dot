;; Custom file setup
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerr)

;; Fix GNU Elpa issue with HTTPS
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Package setup
(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Auto-compile elisp to byte code
(use-package auto-compile
  :init
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Resolve lag caused by icons
(setq inhibit-compacting-font-caches t)

;; UTF-8 forever <3
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Sensible defaults
(load-file "~/.emacs.d/sensible-defaults.el/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

;; Distraction free
(menu-bar-mode 1)

(defun yuki/frame-mods (frame)
  (select-frame frame)
  (when window-system
    (fringe-mode '(nil . nil))
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (modify-frame-parameters frame
                             '((vertical-scroll-bars . nil)
                               (horizontal-scroll-bars . nil)))))

(add-hook 'after-make-frame-functions 'yuki/frame-mods)

(when window-system
  (yuki/frame-mods (selected-frame)))

;; Visual aid
(global-hl-line-mode t)
(column-number-mode t)

;; Show parentheses
(setq show-paren-style 'expression)
(add-hook 'prog-mode-hook 'show-paren-mode)

;; Spaces only
(setq-default indent-tabs-mode nil)

;; Whoami
(setq user-full-name "Muhammad Kaisar Arkhan"
      user-mail-address "hi@yukiisbo.red")

;; Automatically update packages
(use-package auto-package-update
  :config
  (add-hook 'after-init-hook 'auto-package-update-maybe))

;; Get variables from shell
(use-package exec-path-from-shell
  :if window-system
  :init
  (setq-default exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  :config
  (exec-path-from-shell-initialize))

;; Spell checking with hunspell/ispell
(if (executable-find "hunspell")
    (setq ispell-program-name "hunspell")
  (if (executable-find "ispell")
      (setq ispell-program-name "ispell")
    (message "Please install hunspell/ispell for spell checking.")))

;; The superior completion front-end
(use-package ivy
  :bind
  ("C-c C-r" . ivy-resume)
  :init
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t)
  :config
  (add-hook 'after-init-hook 'ivy-mode))

;; The superior isearch
(use-package swiper
  :bind
  ("C-s" . swiper)
  ("C-r" . swiper))

;; Use the superior completion front end
(use-package counsel
  :bind
  ("M-x"     . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("<f1> f"  . counsel-describe-function)
  ("<f2> v"  . counsel-describe-variable)
  ("<f1> l"  . counsel-find-library)
  ("<f1> i"  . counsel-info-lookup-symbol)
  ("<f2> u"  . counsel-unicode-char)
  :config
  (define-key read-expression-map
    (kbd "C-r") 'counsel-expression-history))

;; The silver searcher
(use-package ag
  :after counsel
  :bind
  ("C-c k" . counsel-ag))

;; Display available keybindings in popup
(use-package which-key
  :init
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode t))

;; Easier window management
(use-package switch-window
  :bind
  ("C-x o" . switch-window)
  ("C-x 1" . switch-window-then-maximize)
  ("C-x 2" . switch-window-then-split-below)
  ("C-x 3" . switch-window-then-split-right)
  ("C-x 0" . switch-window-then-delete)
  :init
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))
  (setq switch-window-minibuffer-shortcut ?z))

;; Tree
(use-package neotree
  :bind
  ("C-x f" . neotree-toggle))


;; Identation guides
(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character)
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; A Better Git interface
(use-package magit
  :bind
  ("C-x g" . magit-status))

;; Auto completion interface
(use-package company
  :init
  (setq company-tooltip-align-annotations t)
  :config
  (add-hook 'after-init-hook 'global-company-mode)

  (defun yuki/spelling-setup ()
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends 'company-ispell))
  (add-hook 'yuki/spelling-setup 'text-mode-hook-setup)
  (add-hook 'yuki/spelling-setup 'org-mode-hook-setup)
  (add-hook 'yuki/spelling-setup 'markdown-mode-hook-setup))

;; Automatic whitespace cleanup
(use-package whitespace-cleanup-mode
  :config
  (add-hook 'after-init-hook 'global-whitespace-cleanup-mode))

;; Project awareness
(use-package projectile
  :ensure projectile-git-autofetch
  :bind (:map projectile-mode-map)
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (add-hook 'after-init-hook 'projectile-mode))

;; Cheat sheets
(use-package cheat-sh)

;; Modern on-the-fly syntax checking
(use-package flycheck
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (add-hook 'after-init-hook 'global-flycheck-mode))

;; Persistent Undo
(use-package undo-tree
  :config
  (unless (file-directory-p "~/.emacs.d/undo")
    (make-directory "~/.emacs.d/undo"))
  (setq undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo")))
  (setq undo-tree-auto-save-history t))

;; Highlight TODOs
(use-package hl-todo
  :config
  (add-hook 'after-init-hook 'global-hl-todo-mode))

;; Highlight version control diffs
(use-package diff-hl
  :config
  (add-hook 'after-init-hook 'global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; Show colors for rgb(rr,gg,bb) and #rrggbb
(use-package rainbow-mode
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; IMenu Anywhere
(use-package imenu-anywhere
  :bind
  ("C-c ." . imenu-anywhere))

;; Lisp
(use-package parinfer
  :ensure paredit
  :bind ("C-," . parinfer-toggle-mode)
  :hook
  (clojure-mode . parinfer-mode)
  (emacs-lisp-mode . parinfer-mode)
  (common-lisp-mode . parinfer-mode)
  (scheme-mode . parinfer-mode)
  (lisp-mode . parinfer-mode)
  :init
  (setq parinfer-extensions '(default pretty-parents paredit smart-tab smart-yank)))

;; Multiple cursors
(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("C-c c" . mc/edit-lines))
  :config
  (define-key mc/keymap (kbd "<return>") nil))

;; Go to char
(use-package iy-go-to-char
  :bind
  (("C-c p" . iy-go-to-char-backward)
   ("C-c n" . iy-go-to-char)))

;; DOOM themes
(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-day t)
  (add-hook 'after-init-hook 'doom-themes-neotree-config)
  (add-hook 'after-init-hook 'doom-themes-org-config)
  (add-hook 'after-init-hook 'doom-themes-visual-bell-config))

;; DOOM modeline
(use-package doom-modeline
  :config
  (add-hook 'after-init-hook 'doom-modeline-mode))

;; Dashboard
(use-package dashboard
  :init
  (setq dashboard-startup-banner "~/.emacs.d/waifu_render.png"
	dashboard-banner-logo-title "Hi Yuki, Welcome to GNU Emacs."
	dashboard-center-content t
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-set-navigator t
	dashboard-items '((recents . 5)
			  (projects . 5)
			  (registers . 5)))
  :config
  (dashboard-setup-startup-hook))

;; All the Icons
(use-package all-the-icons
  :ensure all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; PDF tools
(use-package pdf-tools
  :mode "\\.pdf\\'")

;; sublimity
(use-package sublimity
  :init
  (setq sublimity-scroll-weight 10
	sublimity-scroll-drift-length 5)
  :config
  (require 'sublimity-scroll)
  (add-hook 'after-init-hook 'sublimity-mode))

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml")

;; Go
(use-package go-mode
  :mode "\\.go\\'"
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

;; Protocol Buffers
(use-package protobuf-mode)

;; Dockerfile
(use-package dockerfile-mode)

;; HTML
(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)))

;; Markdown
(use-package markdown-mode
  :mode "\\.md\\'"
  :init
  (setq markdown-command "multimarkdown"))

;; Use Python 3 ffs
(if (executable-find "python3")
    (setq python-shell-interpreter "python3")
  (if (executable-find "python3.7")
      (setq python-shell-interpreter "python3.7")
    (message "Failed to find Python 3 intepreter")))

(setq flycheck-python-pycompile-executable python-shell-interpreter
      flycheck-python-flake8-executable python-shell-interpreter
      flycheck-python-pylint-executable python-shell-interpreter)

;; Lua
(use-package lua-mode
  :mode "\\.lua\\'")

;; Org
(use-package org
  :ensure org-plus-contrib
  :init
  (setq org-src-tab-acts-natively t
	org-src-fontify-natively t
	org-latex-to-pdf-process '("xelatex -interaction nonstopmode %f"
				   "xelatex -interaction nonstopmode %f")
	org-latex-listings 'minted
	org-confirm-babel-evaluate nil
	org-export-with-smart-quotes t
	org-ellipsis "⤵"
	initial-major-mode 'org-mode)
  :config
  (require 'org-tempo)
  (require 'ox-md)
  (require 'ox-beamer)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (dot . t)
     (gnuplot . t)))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (eval-after-load 'ox '(require 'ox-koma-letter)))

(use-package graphviz-dot-mode
  :config
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'")

(use-package flycheck-rust
  :init
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)))

(use-package flycheck-inline
  :init
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook 'flycheck-inline-mode)))

(use-package racer
  :config
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (define-key rust-mode-map (kbd "TAB") 'company-indent-or-complete-common))

(use-package cargo
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package flycheck-grammalecte
  :init
  (setq flycheck-grammalecte-enabled-modes
	'(org-mode text-mode mail-mode latex-mode markdown-mode mu4e-compose-mode)
	flycheck-grammalecte-report-spellcheck t))

;; PHP
(use-package php-mode)

;; Nginx
(use-package nginx-mode)

(if (file-directory-p "/usr/local/share/emacs/site-lisp/mu4e")
    (setq mu4e-directory "/usr/local/share/emacs/site-lisp/mu4e")
  (setq mu4e-directory "/usr/share/emacs/site-lisp/mu4e"))

(when (file-directory-p mu4e-directory)
  (add-to-list 'load-path mu4e-directory)
  (require 'mu4e)
  (if (file-exists-p "~/.emacs.d/email-setup.el")
      (load-file "~/.emacs.d/email-setup.el")))
