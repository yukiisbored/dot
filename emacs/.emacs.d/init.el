;; Custom file setup
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerr)

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

;; UTF-8 forever <3
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Improved scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq-default tab-always-indent nil
	      indent-tabs-mode nil)

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

;; Visual aid
(global-hl-line-mode t)
(column-number-mode t)

(setq ispell-program-name "hunspell")

;; Show parentheses
(setq show-paren-style 'expression)
(add-hook 'prog-mode-hook 'show-paren-mode)

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
  (setq exec-path-from-shell-variables '("PATH"
					 "GOPATH"))
  :config
  (exec-path-from-shell-initialize))

;; Material theme
(use-package material-theme
  :config
  (load-theme 'material-light t))

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
  ("C-x M-f" . neotree-toggle))

;; A Better Git interface
(use-package magit
  :bind
  ("C-x g" . magit-status))

;; Auto completion interface
(use-package auto-complete
  :hook (prog-mode . auto-complete-mode)
  :config
  (ac-config-default))

;; Automatic whitespace cleanup
(use-package whitespace-cleanup-mode
  :config
  (add-hook 'after-init-hook 'global-whitespace-cleanup-mode))

;; Project awareness
(use-package projectile
  :bind (:map projectile-mode-map)
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (add-hook 'after-init-hook 'projectile-mode))

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
  :bind ("C-," . parinfer-toggle-mode)
  :hook
  (clojure-mode . parinfer-mode)
  (emacs-lisp-mode . parinfer-mode)
  (common-lisp-mode . parinfer-mode)
  (scheme-mode . parinfer-mode)
  (lisp-mode . parinfer-mode)
  :init
  (setq parinfer-extensions '(default pretty-parents paredit
			       smart-tab smart-yank)))

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml")

;; Go
(use-package go-mode
  :mode "\\.go\\'"
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-autocomplete)

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

;; Org
(use-package org
  :ensure org-plus-contrib
  :init
  (setq org-src-tab-acts-natively t)
  (setq org-latex-to-pdf-process
        '("xelatex -interaction nonstopmode %f"
          "xelatex -interaction nonstopmode %f"))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  (eval-after-load 'ox '(require 'ox-koma-letter)))

;; Rust
(use-package rust-mode)

(use-package flycheck-rust
  :init
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)))

(use-package flycheck-inline
  :init
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook 'flycheck-inline-mode)))

;; mu4e
(setq mu4e-directory "/usr/local/share/emacs/site-lisp/mu4e")
(when (file-directory-p mu4e-directory)
  (add-to-list 'load-path mu4e-directory)
  (require 'mu4e)

  (setq mail-user-agent 'mu4e-user-agent
        mu4e-maildir "~/Maildir"
        mu4e-sent-folder "/Personal/Sent"
        mu4e-drafts-folder "/Personal/Drafts"
        mu4e-trash-folder "/Personal/Trash"
        mu4e-get-mail-command "offlineimap"
        mu4e-compose-signature "yukiisbo.red"
        message-kill-buffer-on-exit t
        message-send-mail-function 'sendmail-send-it)

  (defvar my-mu4e-account-alist
    '(("Personal"
       (mu4e-sent-folder "/Personal/Sent")
       (mu4e-drafts-folder "/Personal/Drafts")
       (mu4e-trash-folder "/Personal/Trash")
       (user-mail-adress "hi@yukiisbo.red"))))

  (defun yuki/mu4e-set-account ()
    (let* ((account
            (if mu4e-compose-parent-message
                (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                  (string-match "/\\(.*?\\)/" maildir)
                  (match-string 1 maildir))
              (completing-read (format "Compose with account: (%s) "
                                       (mapconcat #'(lambda (var) (car var))
                                                  my-mu4e-account-alist "/"))
                               (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                               nil t nil nil (caar my-mu4e-account-alist))))
           (account-vars (cdr (assoc account my-mu4e-account-alist))))
      (if account-vars
          (mapc #'(lambda (var)
                    (set (car var) (cadr var)))
                account-vars)
        (error "No email account found"))))

  (add-hook 'mu4e-compose-pre-hook 'yuki/mu4e-set-account))
