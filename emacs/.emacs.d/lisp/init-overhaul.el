;;; init-overhaul.el --- Make Emacs even more powerful

;; Focus follow mouse
(setq mouse-autoselect-window t
      focus-follows-mouse t)

(defun yuki/frame-mods (frame)
  (select-frame frame)
  (when window-system
    (fringe-mode '(nil . nil))
    (menu-bar-mode 1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (modify-frame-parameters frame
                             `((vertical-scroll-bars . nil)
                               (horizontal-scroll-bars . nil)))))

(add-hook 'after-make-frame-functions 'yuki/frame-mods)
(add-hook 'after-init-hook
          (lambda ()
            (when window-system
              (yuki/frame-mods (selected-frame)))))

;; The superior completion front-end
(use-package helm
  :bind
  ("C-h a"   . helm-apropos)
  ("C-h f"   . helm-apropos)
  ("C-h r"   . helm-info-emacs)
  ("C-x C-f" . helm-find-files)
  ("M-x"     . helm-M-x)
  ("C-x b"   . helm-mini)
  :init
  (setq helm-split-window-in-side-p t    ;; Required to place helm at the bottom
        helm-mode-fuzzy-match       t    ;; Enable fuzzy matching
        helm-display-header-line    nil) ;; Do not display the header line

  ;; Display helm at the bottom
  (add-to-list 'display-buffer-alist
               '("\\`\\*helm.*\\*\\'"
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4)))

  (add-hook 'after-init-hook
            (lambda ()
              (helm-mode t))))

;; The superior isearch
(use-package swiper-helm
  :bind
  ("C-s" . swiper))

;; The Silver Searcher
(use-package helm-ag)

;; Display available keybindings in popup
(use-package which-key
  :init
  (setq which-key-idle-delay 0.5)
  (add-hook 'after-init-hook
	    (lambda ()
	      (which-key-mode t))))

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

;; Dashboard
(use-package dashboard
  :init
  (setq initial-buffer-choice "*dashboard*")
  (setq dashboard-startup-banner (expand-file-name "dashboard_banner.png" user-emacs-directory)
        dashboard-banner-logo-title "Hi Yuki, Welcome to GNU Emacs."
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-navigator t
        dashboard-items `((recents . 5)
			  (projects . 5)
			  (registers . 5)))
  (dashboard-setup-startup-hook)
  (add-hook 'dashboard-mode-hook (lambda ()
                                   (interactive)
                                   (set-buffer-modified-p nil))))

;; All the Icons
(use-package all-the-icons)

;; All the Icons in Helm
(use-package helm-icons
  :after helm all-the-icons
  :init
  (setq helm-icons-provider 'all-the-icons)
  (add-hook 'after-init-hook (lambda ()
                               (helm-icons-enable))))

;; All the icons in Dired
(use-package all-the-icons-dired
  :after all-the-icons
  :hook
  (dired-mode . all-the-icons-dired-mode))


;; Makes it clear where the fuck you're on(tm)
(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'fill)
  (add-hook 'prog-mode-hook #'highlight-indent-guides-mode))

;; A Better Git interface than git(1)
(use-package magit
  :bind
  ("C-x g" . magit-status))

;; Automatic whitespace cleanup
(use-package whitespace-cleanup-mode
  :init
  (add-hook 'after-init-hook
            (lambda ()
              (global-whitespace-cleanup-mode t))))

;; Persistent Undo
(use-package undo-tree
  :config
  (unless (file-directory-p "~/.emacs.d/undo")
    (make-directory "~/.emacs.d/undo"))
  (setq undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo")))
  (setq undo-tree-auto-save-history t))

;; Highlight TODOs
(use-package hl-todo
  :init
  (add-hook 'after-init-hook 'global-hl-todo-mode))

;; Highlight version control diffs
(use-package diff-hl
  :hook
  (dired-mode . diff-hl-dired-mode)
  :init
  (add-hook 'after-init-hook 'global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; Show colors for rgb(rr,gg,bb) and #rrggbb
(use-package rainbow-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-mode))

;; IMenu Anywhere
(use-package imenu-anywhere
  :bind
  ("C-c ." . imenu-anywhere))

;; Multiple cursors
(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("C-c c" . mc/edit-lines))
  :config
  (define-key mc/keymap (kbd "<return>") nil))

;; Awesome code templating system
(use-package yasnippet
  :init
  (setq yas-snippet-dirs `("~/.emacs.d/snippets"))
  (add-hook 'after-init-hook 'yas-global-mode))

;; Snippets for YASnippet
(use-package yasnippet-snippets)

;; Completion system
(use-package company
  :bind
  ("C-c y" . company-yasnippet)
  :init
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 1
        company-idle-delay 0.0)
  (add-hook 'after-init-hook
            (lambda ()
              (global-company-mode t))))

(use-package company-quickhelp
  :after company
  :hook
  (company-mode . company-quickhelp-mode))

;; Flycheck -- Better than flymake
(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package flycheck-inline
  :hook
  (flycheck-mode . flycheck-inline-mode))

;; Projectile
(use-package projectile
  :init
  (setq projectile-completion-system 'helm)
  (add-hook 'after-init-hook
            (lambda ()
              (projectile-mode t))))

;; Use git for Projectile by default
(use-package projectile-git-autofetch)

;; Treemacs
(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind
  ("M-0"       . treemacs-select-window)
  ("C-x t 1"   . treemacs-delete-other-windows)
  ("C-x t t"   . treemacs)
  ("C-x t B"   . treemacs-bookmark)
  ("C-x t C-t" . treemacs-find-file)
  ("C-x t M-t" . treemacs-find-tag))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-magit
  :after treemacs magit)

;; Editorconfig
(use-package editorconfig
  :init
  (add-hook 'after-init-hook
            (lambda ()
              (editorconfig-mode t))))

;; VTerm
(use-package vterm
  :init
  (global-set-key (kbd "<s-return>") 'vterm)
  (global-set-key (kbd "C-x RET RET") 'vterm)
  (setq vterm-buffer-name-string "*vterm: %s*"))

;; direnv integration
(use-package direnv
 :init
 (add-hook 'after-init-hook
           (lambda ()
             (direnv-mode t))))

;; Clock in Emacs
(setq display-time-24hr-format t
      display-time-format "%H:%M - %d %B %Y")
(display-time-mode 1)

;; Doom Emacs Theme
(use-package doom-themes
  :init
  ;; Increase contrast for some stuff
  ;; Some people really need to read W3C WCAG
  (setq doom-opera-light-brighter-comments t)

  (custom-set-faces
   '(helm-ff-directory ((t (:extend t :foreground "#3b6ea8"))))
   '(helm-ff-symlink ((t (:inherit font-lock-comment-face :extend t :foreground "#842879")))))

  (add-hook 'after-init-hook
            (lambda ()
              (load-theme 'doom-opera-light t)
              (doom-themes-visual-bell-config)
              (doom-themes-treemacs-config)
              (doom-themes-org-config))))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(provide 'init-overhaul)
