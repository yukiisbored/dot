(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerr)

(eval-and-compile
  (setq frame-inhibit-implied-resize   t
        inhibit-compacting-font-caches t

        gc-cons-threshold              (* 100 1024 1024)
        read-process-output-max        (* 1024 1024)

        vc-follow-symlinks             t

        indent-tabs-mode               nil
        sentence-end-double-space      nil
        require-final-newline          t

        dired-listing-switched         "-alh"

        show-paren-delay               0.0
        show-paren-style               'expression

        mouse-yank-at-point            t

        inhibit-startup-message        t
        initial-scratch-message        nil

        auto-save-default              nil
        make-backup-files              nil

        mouse-autoselect-window        t
        focus-follows-mouse            t)

  (defun yuki/ask-make-parent-dir ()
    "Make parent directory if it does not exist"
    (when buffer-file-name
      (let ((dir (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p dir))
		   (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
	  (make-directory dir t)))))

  (defun yuki/boot-server ()
    (require 'server)
    (unless (server-running-p) (server-start)))

  (fset #'yes-or-no-p #'y-or-n-p)

  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  (add-hook 'text-mode-hook 'hl-line-mode)

  (add-hook 'prog-mode-hook 'hl-line-mode)
  (add-hook 'prog-mode-hook 'subword-mode)

  (add-hook 'before-save-hook 'yuki/ask-make-parent-dir)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

  (add-hook 'after-init-hook 'transient-mark-mode)
  (add-hook 'after-init-hook 'delete-selection-mode)
  (add-hook 'after-init-hook 'global-font-lock-mode)
  (add-hook 'after-init-hook 'global-auto-revert-mode)
  (add-hook 'after-init-hook 'show-paren-mode)
  (add-hook 'after-init-hook 'column-number-mode)
  (add-hook 'after-init-hook 'yuki/boot-server))

(eval-and-compile
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
              (yuki/frame-mods (selected-frame)))))

(eval-and-compile
  (setq package-archives
        `(("gnu" . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")))

  (add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp")
  (add-to-list 'load-path "~/.emacs.d/site-lisp")

  (setq package-enable-at-startup nil)
  (package-initialize)

  ;; Setup `use-package'
  (setq use-package-verbose t
        use-package-always-ensure t
        use-package-always-defer t
        use-package-expand-minimally t)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(require 'use-package)

(use-package diminish
  :diminish subword-mode eldoc-mode)

(use-package bind-key)
(use-package gnu-elpa-keyring-update)

(use-package paradox
  :custom ((paradox-execute-asynchronously t)
           (paradox-github-token           t)
           (paradox-display-star-count     nil))
  :init
  (defun yuki/paradox-enable (&rest _)
    "Enable paradox, overriding the default package-menu."
    (paradox-enable))

  (advice-add 'list-packages :before 'yuki/paradox-enable))

(use-package exec-path-from-shell
  :if window-system
  :hook ((after-init . exec-path-from-shell-initialize))
  :custom ((exec-path-from-shell-check-startup-files nil)
           (exec-path-from-shell-variables '("PATH" "GOPATH" "NIX_PATH"))))

(use-package ivy
  :diminish
  :bind (("C-c C-r" . ivy-resume))
  :hook ((after-init . ivy-mode))
  :custom ((ivy-use-virtual-buffers      t)
           (ivy-initial-inputs-alist     nil)
           (enable-recursive-minibuffers t)))

(use-package ivy-rich
  :hook ((ivy-mode . ivy-rich-mode))
  :custom ((ivy-rich-path-style 'abbrev)))

(use-package all-the-icons-ivy-rich
  :if window-system
  :hook ((ivy-mode . all-the-icons-ivy-rich-mode)))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

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

(use-package ag
  :after counsel
  :bind (("C-c k" . counsel-ag)))

(use-package which-key
  :diminish
  :hook ((after-init . which-key-mode))
  :custom ((which-key-idle-delay 0.5)))

(use-package switch-window
  :bind (("C-x o" . switch-window)
         ("C-x 1" . switch-window-then-maximize)
         ("C-x 2" . switch-window-then-split-below)
         ("C-x 3" . switch-window-then-split-right)
         ("C-x 0" . switch-window-then-delete))
  :custom ((switch-window-shortcut-style      'qwerty)
           (switch-window-qwerty-shortcuts    '("a" "s" "d" "f"
                                                "j" "k" "l" ";"
                                                "w" "e" "i" "o"))
           (switch-window-minibuffer-shortcut ?z)))

(use-package dashboard
  :custom ((dashboard-startup-banner    (expand-file-name "assets/dashboard_banner.png" user-emacs-directory))
	   (dashboard-banner-logo-title "Hi Yuki, Welcome to GNU Emacs.")
	   (dashboard-center-content    t)
	   (dashboard-set-heading-icons t)
	   (dashboard-set-file-icons    t)
	   (dashboard-set-navigator     t)
	   (dashboard-items             '((recents . 5)
			                  (projects . 5)
			                  (registers . 5)))
	   (dashboard-page-separator    "\n\n\n"))
  :init
  (dashboard-setup-startup-hook))

(use-package all-the-icons
  :if window-system)

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package whitespace-cleanup-mode
  :diminish
  :hook ((after-init . global-whitespace-cleanup-mode)))

(use-package undo-tree
  :custom ((undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo")))
	   (undo-tree-auto-save-history       t))
  :init
  (unless (file-directory-p "~/.emacs.d/undo")
    (make-directory "~/.emacs.d/undo")))

(use-package hl-todo
  :hook (after-init . global-hl-todo-mode))

(use-package diff-hl
  :hook ((dired-mode         . diff-hl-dired-mode)
         (after-init         . global-diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package rainbow-mode
  :diminish
  :hook (prog-mode . rainbow-mode))

(use-package imenu-anywhere
  :bind (("C-c ." . imenu-anywhere)))

(use-package multiple-cursors
  :bind (("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c c"   . mc/edit-lines)
         :map mc/keymap
         ([return]  . nil)))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook ((after-init . yas-global-mode))
  :custom ((yas-snippet-dirs '("~/.emacs.d/snippets"))))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package company
  :diminish
  :hook ((after-init . global-company-mode))
  :bind (("C-c y" . company-yasnippet))
  :custom ((company-tooltip-align-annotations t)
           (company-minimum-prefix-length 1)
           (company-idle-delay 0.0)))

(use-package company-quickhelp
  :after company
  :hook ((company-mode . company-quickhelp-mode)))

(use-package flycheck
  :diminish
  :hook ((after-init . global-flycheck-mode))
  :custom ((flycheck-disabled-checkers '(emacs-lisp-checkdoc))))

(use-package flycheck-inline
  :hook ((flycheck-mode . flycheck-inline-mode)))

(use-package projectile
  :hook ((after-init . projectile-mode))
  :custom ((projectile-completion-system 'ivy)
           (projectile-keymap-prefix     (kbd "C-c p"))))

(use-package projectile-git-autofetch
  :after projectile)

(use-package editorconfig
  :diminish
  :hook ((after-init . editorconfig-mode)))

(use-package vterm
  :bind (("C-x RET RET" . vterm))
  :custom ((vterm-buffer-name-string "*vterm: %s*")))

(use-package envrc
  :bind
  (("C-c e" . envrc-command-map))
  :hook ((after-init . envrc-global-mode)))

(use-package format-all
  :diminish
  :hook (prog-mode . format-all-mode))

(use-package modus-themes
  :custom ((modus-themes-mixed-fonts t)
	   (modus-themes-syntax '(yellow-comments green-strings alt-syntax))
	   (modus-themes-scale-headings t))
  :init (modus-themes-load-themes)
  :config (modus-themes-load-operandi))

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration)
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
  :custom ((lsp-enable-file-watchers nil)
           (lsp-keymap-prefix        "C-c l")))

(use-package lsp-ui
  :after lsp-mode)

(use-package lsp-ivy
  :after lsp-mode ivy)

(use-package dap-mode
  :hook ((lsp-mode . dap-mode)
         (lsp-mode . dap-ui-mode)))

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
  :custom ((web-mode-engines-alist '(("django" . "\\.j?j2\\'"))))
  :init
  (define-derived-mode svelte-mode web-mode "svelte-mode"))

(use-package pug-mode)

(use-package yaml-mode
  :mode "\\.ya?ml")

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package cargo
  :hook ((rust-mode . cargo-minor-mode)))

(use-package php-mode)

(use-package nginx-mode)

(use-package markdown-mode
  :mode "\\.md\\'"
  :custom ((markdown-command "multimarkdown")))

(use-package dockerfile-mode)

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package go-mode
  :mode "\\.go\\'"
  :hook ((before-save . gofmt-before-save)))

(use-package elixir-mode)

(use-package haskell-mode
  :hook ((haskell-mode . interactive-haskell-mode)))

(use-package lsp-haskell
  :hook ((haskell-mode . lsp)
         (haskell-literate-mode . lsp)))

(use-package typescript-mode)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package clojure-mode)
(use-package cider)

(use-package dhall-mode
  :custom ((dhall-format-arguments (\` ("--ascii")))
	   (dhall-use-header-line nil)))

(use-package org
  :ensure org-plus-contrib
  :hook
  ((org-mode . (lambda () (hl-line-mode nil)))
   (org-mode . auto-fill-mode))
  :custom ((initial-major-mode                  'org-mode)
	   (org-startup-indented                t)
	   (org-pretty-entities                 t)
	   (org-hide-emphasis-markers           t)
	   (org-fontify-whole-heading-line      t)
	   (org-fontify-done-headline           t)
	   (org-fontify-quote-and-verse-blocks  t)
	   (org-ellipsis                        "  ")

	   (org-src-tab-acts-natively           t)
	   (org-src-fontify-natively            t)
	   (org-src-window-setup                'current-window)

	   (org-latex-to-pdf-process            '("xelatex -interaction nonstopmode %f"
						  "xelatex -interaction nonstopmode %f"))
	   (org-latex-listings                  'minted)

	   (org-confirm-babel-evaluate          nil)

	   (org-export-with-smart-quotes        t)
	   (org-export-with-section-numbers     nil)
	   (org-export-with-toc                 nil)

	   (org-startup-with-inline-images      t)

	   (org-html-divs                       '((preamble  "header" "top")
						  (content   "main"   "content")
						  (postamble "footer" "postamble")))
	   (org-html-container-element          "section")
	   (org-html-validation-link            nil)
	   (org-html-head-include-default-style nil)
	   (org-html-html5-fancy                t)
	   (org-html-doctype                    "html5"))
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
  :custom ((org-roam-directory (file-truename "~/roam"))
	   (org-roam-v2-ack t))
  :config
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :commands (org-roam-ui-mode)
  :custom ((org-roam-ui-sync-theme t)
	   (org-roam-ui-follow t)
	   (org-roam-ui-update-on-save t)
	   (org-roam-ui-open-on-start t)))

(use-package htmlize)
