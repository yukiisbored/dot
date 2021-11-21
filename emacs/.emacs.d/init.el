;; Do not alter the startup file for customize variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerr)

;; Fix GnuTLS
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Do not alter fonts
(setq frame-inhibit-implied-resize t)

;; Set default directory to $HOME
(setq default-directory "~/")

;; Increase GC threshold to 100 MB
(setq gc-cons-threshold 100000000)

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;; Read at most 1 MB from processes
(setq read-process-output-max (* 1024 1024))

;; Delete trailing whitespace everytime file is saved
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Treat camel case subwords as separate words in programming modes
(add-hook 'prog-mode-hook #'subword-mode)

;; Always follow symlinks
(setq vc-follow-symlinks t)

;; Make scripts executable on save
(add-hook 'after-save-hook
	  #'executable-make-buffer-file-executable-if-script-p)

;; Don't add two spaces after periods.
(setq sentence-end-double-space nil)

;; Make parent directory if they don't exist
(defun yuki/ask-make-parent-dir ()
  (when buffer-file-name
    (let ((dir (file-name-directory buffer-file-name)))
      (when (and (not (file-exists-p dir))
		 (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
	(make-directory dir t)))))

(add-hook 'before-save-hook #'yuki/ask-make-parent-dir)

;; Enable transient mark mode
(add-hook 'after-init-hook
	  (lambda ()
	    (transient-mark-mode t)))

;; Delete selected text if something is typed
(add-hook 'after-init-hook
	  (lambda ()
	    (delete-selection-mode t)))

;; Append newline at the end of files
(setq require-final-newline t)

;; Use `y/n?' instead of `yes/no?'
(fset #'yes-or-no-p #'y-or-n-p)

;; Use human-readable units in dired
(setq-default dired-listing-switched "-alh")

;; Turn on syntax highlighting whenever possible
(add-hook 'after-init-hook
	  (lambda ()
	    (global-font-lock-mode t)))

;; Always refresh file when something is changed
(add-hook 'after-init-hook
	  (lambda ()
	    (global-auto-revert-mode t)))

;; Visually indicate matching pairs of parentheses.
(setq show-paren-delay 0.0)
(setq show-paren-style 'expression)

(add-hook 'after-init-hook
	  (lambda ()
	    (show-paren-mode t)))

;; 80 COLUMNS IS STANDARD
(setq fill-column 80)

;; Yank to where the cursor is
(setq mouse-yank-at-point t)

;; Backup to temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; UTF-8 forever
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Visual aid
(add-hook 'after-init-hook 'column-number-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'text-mode-hook 'hl-line-mode)

;; Spaces are superior, fuck you.
(setq-default indent-tabs-mode nil)

;; Empty startup
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Disable backups and auto-saves
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Use SSH, not SCP for Tramp
(setq tramp-default-method "ssh")

;; Do not emit warnings
(setq warning-suppress-log-types '((comp))
      warning-suppress-types '((comp)))

;; Setup hooks to boot server
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p) (server-start))))

;; Declare package archives
(setq package-archives
  `(("gnu" . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")))

;; Declare local load paths
(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Start Emacs package manager
(setq package-enable-at-startup nil)
(package-initialize)

;; Setup `use-package'
(setq use-package-verbose t
      use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package diminish)
(use-package bind-key)
(use-package gnu-elpa-keyring-update)

;; A modern Packages Menu
(use-package paradox
  :init
  (setq paradox-execute-asynchronously t
        paradox-github-token t
        paradox-display-star-count nil)

  ;; Replace default `list-packages'
  (defun yuki/paradox-enable (&rest _)
    "Enable paradox, overriding the default package-menu."
    (paradox-enable))
  (advice-add #'list-packages :before #'yuki/paradox-enable))

(use-package exec-path-from-shell
  :if window-system
  :hook ((after-init . exec-path-from-shell-initialize))
  :init
  (setq-default exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "NIX_PATH")))

;; Focus follow mouse
(setq mouse-autoselect-window t
      focus-follows-mouse t)

(defun yuki/frame-mods (frame)
  (select-frame frame)
  (menu-bar-mode -1)
  (when window-system
    (tooltip-mode 0)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (set-face-attribute 'default nil :font "Fira Code-12" )
    (set-frame-font "Fira Code-12" nil t)))

(add-hook 'after-make-frame-functions 'yuki/frame-mods)
(add-hook 'after-init-hook
          (lambda ()
            (yuki/frame-mods (selected-frame))))

;; The superior completion front-end
(use-package ivy
  :bind (("C-c C-r" . ivy-resume))
  :hook ((after-init . ivy-mode))
  :init
  (setq ivy-use-virtual-buffers t
        ivy-initial-inputs-alist nil
	enable-recursive-minibuffers t))

;; More friendly for ivy
(use-package ivy-rich
  :hook ((ivy-mode . ivy-rich-mode))
  :init
  (setq ivy-rich-path-style 'abbrev))

;; Icons for ivy-rich
(use-package all-the-icons-ivy-rich
  :if window-system
  :hook ((ivy-mode . all-the-icons-ivy-rich-mode)))

;; The superior isearch
(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;; Use the superior completion front end
(use-package counsel
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f"  . counsel-describe-function)
         ("<f2> v"  . counsel-describe-variable)
         ("<f1> l"  . counsel-find-library)
         ("<f1> i"  . counsel-info-lookup-symbol)
         ("<f2> u"  . counsel-unicode-char)
         :map read-expression-map
         ("C-r"     . counsel-expression-history)))

;; The silver searcher
(use-package ag
  :after counsel
  :bind (("C-c k" . counsel-ag)))

;; Display available keybindings in popup
(use-package which-key
  :hook ((after-init . which-key-mode))
  :init
  (setq which-key-idle-delay 0.5))

;; Easier window management
(use-package switch-window
  :bind (("C-x o" . switch-window)
         ("C-x 1" . switch-window-then-maximize)
         ("C-x 2" . switch-window-then-split-below)
         ("C-x 3" . switch-window-then-split-right)
         ("C-x 0" . switch-window-then-delete))
  :init
  (setq switch-window-shortcut-style 'qwerty
        switch-window-qwerty-shortcuts '("a" "s" "d" "f" "j" "k"
                                         "l" ";" "w" "e" "i" "o")
        switch-window-minibuffer-shortcut ?z))

;; Dashboard
(use-package dashboard
  :init
  (setq dashboard-startup-banner (if window-system
                                     (expand-file-name "assets/dashboard_banner.png" user-emacs-directory)
                                   (expand-file-name "assets/dashboard_banner.txt" user-emacs-directory))
        dashboard-banner-logo-title "Hi Yuki, Welcome to GNU Emacs."
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-navigator t
        dashboard-items `((recents . 5)
			  (projects . 5)
			  (registers . 5))
        dashboard-page-separator "\n\n\n")
  (dashboard-setup-startup-hook))

;; All the Icons
(use-package all-the-icons
  :if window-system)

;; A Better Git interface than git(1)
(use-package magit
  :bind (("C-x g" . magit-status)))

;; Automatic whitespace cleanup
(use-package whitespace-cleanup-mode
  :hook ((after-init . global-whitespace-cleanup-mode)))

;; Persistent Undo
(use-package undo-tree
  :init
  (unless (file-directory-p "~/.emacs.d/undo")
    (make-directory "~/.emacs.d/undo"))
  (setq undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo"))
        undo-tree-auto-save-history t))

;; Highlight TODO
(use-package hl-todo
  :hook (after-init . global-hl-todo-mode))

;; Highlight version control diffs
(use-package diff-hl
  :hook ((dired-mode         . diff-hl-dired-mode)
         (after-init         . global-diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

;; Show colors for rgb(rr,gg,bb) and #rrggbb
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; IMenu Anywhere
(use-package imenu-anywhere
  :bind (("C-c ." . imenu-anywhere)))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c c"   . mc/edit-lines)
         :map mc/keymap
         ([return]  . nil)))

;; Awesome code templating system
(use-package yasnippet
  :hook ((after-init . yas-global-mode))
  :init
  (setq yas-snippet-dirs `("~/.emacs.d/snippets")))

;; Snippets for YASnippet
(use-package yasnippet-snippets
  :after yasnippet)

;; Completion system
(use-package company
  :hook ((after-init . global-company-mode))
  :bind (("C-c y" . company-yasnippet))
  :init
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 1
        company-idle-delay 0.0))

(use-package company-quickhelp
  :after company
  :hook ((company-mode . company-quickhelp-mode)))

;; Flycheck -- Better than flymake
(use-package flycheck
  :hook ((after-init . global-flycheck-mode))
  :init
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package flycheck-inline
  :hook ((flycheck-mode . flycheck-inline-mode)))

;; Projectile
(use-package projectile
  :hook ((after-init . projectile-mode))
  :init
  (setq projectile-completion-system 'ivy
        projectile-keymap-prefix (kbd "C-c p")))

;; Use git for Projectile by default
(use-package projectile-git-autofetch
  :after projectile)

;; Editorconfig
(use-package editorconfig
  :hook ((after-init . editorconfig-mode)))

;; VTerm
(use-package vterm
  :bind (("C-x RET RET" . vterm))
  :init
  (setq vterm-buffer-name-string "*vterm: %s*"))

;; direnv integration
(use-package envrc
  :bind
  (("C-c e" . envrc-command-map))
  :hook ((after-init . envrc-global-mode)))

;; Auto formatting
(use-package format-all
  :hook (prog-mode . format-all-mode))

(use-package lsp-mode
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (lsp-mode . lsp-lens-mode)

   (python-mode     . lsp)
   (php-mode        . lsp)
   (go-mode         . lsp)
   (elixir-mode     . lsp)
   (java-mode       . lsp)
   (typescript-mode . lsp)
   (rust-mode       . lsp)
   (svelte-mode     . lsp)
   (dhall-mode      . lsp)
   (purescript-mode . lsp))
  :init
  (setq lsp-enable-file-watchers nil
        lsp-keymap-prefix "C-c l"))

(use-package lsp-ui
  :after lsp-mode)

(use-package lsp-ivy
  :after lsp-mode ivy)

(use-package dap-mode
  :hook
  ((lsp-mode . dap-mode)
   (lsp-mode . dap-ui-mode)))

(use-package lsp-haskell
  :hook
  ((haskell-mode . lsp)
   (haskell-literate-mode . lsp)))

(use-package web-mode
  :mode (("\\.phtml\\'"     . web-mode)
         ("\\.twig\\'"      . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'"   . web-mode)
         ("\\.as[cp]x\\'"   . web-mode)
         ("\\.erb\\'"       . web-mode)
         ("\\.mustache\\'"  . web-mode)
         ("\\.djhtml\\'"    . web-mode)
         ("\\.html?\\'"     . web-mode)
         ("\\.j?j2\\'"      . web-mode)
         ("\\.vue\\'"       . web-mode)
         ("\\.svelte\\'"    . svelte-mode))
  :init
  (setq web-mode-engines-alist '(("django" . "\\.j?j2\\'")))
  (define-derived-mode svelte-mode web-mode "svelte-mode"))

(use-package pug-mode)

(use-package yaml-mode
  :mode "\\.ya?ml")

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package cargo
  :hook ((rust-mode . cargo-minor-mode)))

;; Use Python 3 ffs
(eval-and-compile
  (if (executable-find "python3.7")
      (setq python-shell-interpreter "python3.7")
    (if (executable-find "python3.8")
        (setq python-shell-interpreter "python3.8")
      (setq python-shell-interpreter "python3"))))

(eval-and-compile
  (setq flycheck-python-pycompile-executable python-shell-interpreter)
  (setq flycheck-python-flake8-executable python-shell-interpreter)
  (setq flycheck-python-pylint-executable python-shell-interpreter)
  (setq org-babel-python-command python-shell-interpreter)
  (setq elpy-rpc-python-command python-shell-interpreter)
  (setq python-shell-completion-native-enable 'nil))

(use-package php-mode)

(use-package nginx-mode)

(use-package markdown-mode
  :mode "\\.md\\'"
  :init
  (setq markdown-command "multimarkdown"))

(use-package dockerfile-mode)

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package go-mode
  :mode "\\.go\\'"
  :hook ((before-save . gofmt-before-save)))

(use-package elixir-mode)

(use-package haskell-mode
  :hook ((haskell-mode . interactive-haskell-mode)))

(use-package typescript-mode)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package clojure-mode)
(use-package cider)

(use-package dhall-mode
  :config
  (setq dhall-format-arguments (\` ("--ascii"))
        dhall-use-header-line nil))

(use-package org
  :ensure org-plus-contrib
  :hook
  ((org-mode . (lambda () (hl-line-mode nil)))
   (org-mode . auto-fill-mode))
  :init
  (setq initial-major-mode                  'org-mode

        org-startup-indented                t
        org-pretty-entities                 t
        org-hide-emphasis-markers           t
        org-fontify-whole-heading-line      t
        org-fontify-done-headline           t
        org-fontify-quote-and-verse-blocks  t
        org-ellipsis                        "  "

        org-src-tab-acts-natively           t
        org-src-fontify-natively            t
        org-src-window-setup                'current-window

        org-latex-to-pdf-process            `("xelatex -interaction nonstopmode %f"
                                              "xelatex -interaction nonstopmode %f")
        org-latex-listings                  'minted

        org-confirm-babel-evaluate          nil

        org-export-with-smart-quotes        t
        org-export-with-section-numbers     nil
        org-export-with-toc                 nil

        org-startup-with-inline-images      t

        org-html-divs                       '((preamble  "header" "top")
                                              (content   "main"   "content")
                                              (postamble "footer" "postamble"))
        org-html-container-element          "section"
        org-html-validation-link            nil
        org-html-head-include-default-style nil
        org-html-html5-fancy                t
        org-html-doctype                    "html5")
  :config
  ;; Babel
  (defvar load-language-list `((emacs-lisp . t)
                               (perl       . t)
                               (python     . t)
                               (ruby       . t)
                               (js         . t)
                               (css        . t)
                               (sass       . t)
                               (C          . t)
                               (java       . t)
                               (plantuml   . t)))
  (org-babel-do-load-languages 'org-babel-load-languages load-language-list)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  ;; Publishing
  (require 'ox-publish)
  (require 'ox-rss)

  (defun yuki/format-date-subtitle (file project)
    (format-time-string "Published on %Y-%m-%d" (org-publish-find-date file project)))

  (defun yuki/org-html-close-tag (tag &rest attrs)
    (concat "<" tag " "
            (mapconcat (lambda (attr)
                         (format "%s=\"%s\"" (car attr) (cadr attr)))
                       attrs
                       " ")
            ">"))

  (defun yuki/html-head-extra (file project)
    (let* ((info (cdr project))
           (org-export-options-alist
            `((:title "TITLE" nil nil parse)
              (:date "DATE" nil nil parse)
              (:author "AUTHOR" nil ,(plist-get info :author) space)
              (:description "DESCRIPTION" nil nil newline)
              (:keywords "KEYWORDS" nil nil space)
              (:meta-image "META_IMAGE" nil ,(plist-get info :meta-image) nil)
              (:meta-type "META_TYPE" nil ,(plist-get info :meta-type) nil)))
           (title (org-publish-find-title file project))
           (date (org-publish-find-date file project))
           (author (org-publish-find-property file :author project))
           (description (org-publish-find-property file :description project))
           (link-home (file-name-as-directory (plist-get info :html-link-home)))
           (extension (or (plist-get info :html-extension) org-html-extension))
           (rel-file (org-publish-file-relative-name file info))
           (full-url (concat link-home (file-name-sans-extension rel-file) "." extension))
           (image (concat link-home (org-publish-find-property file :meta-image project)))
           (type (org-publish-find-property file :meta-type project)))
      (mapconcat 'identity
                 `(,(yuki/org-html-close-tag "link" '(rel alternate) '(type application/rss+xml) '(href "rss.xml") '(title "RSS feed"))
                   ,(yuki/org-html-close-tag "meta" '(property og:title) `(content ,title))
                   ,(yuki/org-html-close-tag "meta" '(property og:url) `(content ,full-url))
                   ,(and description
                         (yuki/org-html-close-tag "meta" '(property og:description) `(content ,description)))
                   ,(yuki/org-html-close-tag "meta" '(property og:image) `(content ,image))
                   ,(yuki/org-html-close-tag "meta" '(property og:type) `(content ,type))
                   ,(and (equal type "article")
                         (yuki/org-html-close-tag "meta" '(property article:author) `(content ,author)))
                   ,(and (equal type "article")
                         (yuki/org-html-close-tag "meta" '(property article:published_time) `(content ,(format-time-string "%FT%T%z" date))))

                   ,(yuki/org-html-close-tag "meta" '(property twitter:title) `(content ,title))
                   ,(yuki/org-html-close-tag "meta" '(property twitter:url) `(content ,full-url))
                   ,(yuki/org-html-close-tag "meta" '(property twitter:image) `(content ,image))
                   ,(and description
                         (yuki/org-html-close-tag "meta" '(property twitter:description) `(content ,description)))
                   ,(and description
                         (yuki/org-html-close-tag "meta" '(property twitter:card) '(content summary)))
                   ,(yuki/org-html-close-tag "link" '(rel "stylesheet") '(type "text/css") '(href "https://cdn.jsdelivr.net/gh/tonsky/FiraCode@5.2/distr/fira_code.css"))
                   ,(yuki/org-html-close-tag "link" '(rel "stylesheet") '(type "text/css") '(href "https://code.cdn.mozilla.net/fonts/fira.css"))
                   ,(yuki/org-html-close-tag "link" '(rel "stylesheet") '(type "text/css") '(href "https://gongzhitaao.org/orgcss/org.css"))
                   ,(yuki/org-html-close-tag "link" '(rel "stylesheet") '(type "text/css") '(href "/static/stylesheet.css"))
                   )
                 "\n")))

  (defun yuki/org-html-publish-to-html (plist filename pub-dir)
    (let ((project (cons 'rw plist)))
      (plist-put plist :html-head-extra
                 (yuki/html-head-extra filename project))
      (when (not (equal "index.org" (file-name-nondirectory filename)))
        (plist-put plist :subtitle
                   (yuki/format-date-subtitle filename project)))
      (org-html-publish-to-html plist filename pub-dir)))

  (defun yuki/org-sitemap (title list)
    (concat "#+INCLUDE: ~/org/intro.org\n"
            (org-list-to-org list)))

  (defun yuki/org-sitemap-format (entry style project)
    (cond ((not (directory-name-p entry))
           (format "[[file:%s][%s — %s]]"
                   entry
                   (format-time-string "%Y-%m-%d"
                                       (org-publish-find-date entry project))
                   (org-publish-find-title entry project)))
          ((eq style 'tree)
           ;; Return only last subdir.
           (file-name-nondirectory (directory-file-name entry)))
          (t entry)))

  (defun yuki/org-rss-publish-to-rss (plist filename pub-dir)
    (if (equal "feed.org" (file-name-nondirectory filename))
        (org-rss-publish-to-rss plist filename pub-dir)))

  (defun yuki/org-feed (title sitemap)
    (let* ((title "Yuki's Notes"))
      (concat (format "#+TITLE: %s\n\n" title)
              (org-list-to-subtree
               sitemap
               1
               '(:splice t :istart nil :icount nil
                 :dtstart " " :dtend " ")))))

  (defun yuki/org-feed-format (entry style project)
    (let* ((file (org-publish--expand-file-name entry project))
           (title (org-publish-find-title entry project))
           (date (format-time-string "%Y-%m-%d" (org-publish-find-date entry project)))
           (link (concat (file-name-sans-extension entry) ".html")))
      (with-temp-buffer
        (insert (format "* [[file:%s][%s]]\n" entry title))
        (org-set-property "RSS_PERMALINK" link)
        (org-set-property "RSS_TITLE" title)
        (org-set-property "PUBDATE" date)
        (insert (format "#+INCLUDE: \"%s\" :lines \"3-\"" file))
        (buffer-string))))

  (setq org-publish-project-alist
        (list
         (list "org-posts"
               :base-directory "~/org"
               :exclude (regexp-opt '("index.org" "setup.org" "intro.org" "drafts/" "feed.org"))
               :base-extension "org"
               :publishing-directory "~/org/public/"
               :recursive t
               :publishing-function 'yuki/org-html-publish-to-html
               :headline-levels 3
               :auto-preamble t
               :auto-sitemap t
               :html-link-home "https://notes.yukiisbo.red"
               :html-link-use-abs-url t
               :html-link-org-files-as-html t
               :sitemap-filename "index.org"
               :sitemap-style 'list
               :sitemap-sort-files 'anti-chronologically
               :sitemap-function 'yuki/org-sitemap
               :sitemap-format-entry 'yuki/org-sitemap-format
               :author "Yuki"
               :email "hi@yukiisbo.red"
               :meta-image "static/images/profile.png"
               :meta-type "article")
         (list "org-drafts"
               :base-directory "~/org/drafts/"
               :base-extension "org"
               :publishing-directory "~/org/public/drafts/"
               :auto-preamble t
               :recursive t
               :publishing-function 'yuki/org-html-publish-to-html
               :headline-levels 3
               :auto-preamble t
               :html-link-home "https://notes.yukiisbo.red"
               :html-link-use-abs-url t
               :html-link-org-files-as-html t
               :author "Yuki"
               :email "hi@yukiisbo.red"
               :meta-image "static/images/profile.png"
               :meta-type "article")
         (list "org-feed"
               :base-directory "~/org"
               :exclude (regexp-opt '("index.org" "setup.org" "intro.org" "drafts/" "feed.org"))
               :base-extension "org"
               :publishing-directory "~/org/public/"
               :recursive t
               :publishing-function 'yuki/org-rss-publish-to-rss
               :headline-levels 3
               :auto-sitemap t
               :rss-extension "xml"
               :html-link-home "https://notes.yukiisbo.red"
               :html-link-use-abs-url t
               :html-link-org-files-as-html t
               :sitemap-filename "feed.org"
               :sitemap-title "Yuki's notes"
               :sitemap-style 'list
               :sitemap-sort-files 'anti-chronologically
               :sitemap-function 'yuki/org-feed
               :sitemap-format-entry 'yuki/org-feed-format
               :author "Yuki"
               :email "hi@yukiisbo.red")
         (list "org-static"
               :base-directory "~/org/static"
               :exclude (regexp-opt '("public/"))
               :base-extension (regexp-opt '("css" "js" "png" "jpg" "gif" "webm" "mp4"))
               :publishing-directory "~/org/public/static"
               :recursive t
               :publishing-function 'org-publish-attachment)
         (list "org" :components '("org-posts" "org-drafts" "org-feed" "org-static")))))

(use-package graphviz-dot-mode
  :after org
  :config
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)))

(use-package org-roam
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :custom
  ((org-roam-directory (file-truename "~/roam"))
   (org-roam-v2-ack t))
  :config
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

(use-package simple-httpd)
(use-package websocket)

(use-package org-roam-ui
  :commands (org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package htmlize)
