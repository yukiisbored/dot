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
  (setq initial-buffer-choice "*dashboard*"
        dashboard-startup-banner (expand-file-name "dashboard_banner.png" user-emacs-directory)
        dashboard-banner-logo-title "Hi Yuki, Welcome to GNU Emacs."
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-navigator t
        dashboard-items `((recents . 5)
			  (projects . 5)
			  (registers . 5)))
  (dashboard-setup-startup-hook))

;; All the Icons
(use-package all-the-icons)

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

;; Treemacs
(use-package treemacs
  :bind
  (("M-0"       . treemacs-select-window)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t t"   . treemacs)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag))
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

;; Editorconfig
(use-package editorconfig
  :hook ((after-init . editorconfig-mode)))

;; VTerm
(use-package vterm
  :bind (("C-x RET RET" . vterm))
  :init
  (setq vterm-buffer-name-string "*vterm: %s*"))

;; direnv integration
(use-package direnv
  :hook ((after-init . direnv-mode)))

;; Doom Emacs Theme
(use-package doom-themes
  :hook ((after-init . (lambda () (load-theme 'doom-opera-light t)))
         (after-init . doom-themes-visual-bell-config)
         (after-init . doom-themes-treemacs-config)
         (after-init . doom-themes-org-config))
  :init
  ;; Increase contrast for some stuff
  ;; Some people really need to read W3C WCAG
  (setq doom-opera-light-brighter-comments t)

  (custom-set-faces
   '(helm-ff-directory ((t (:extend t :foreground "#3b6ea8"))))
   '(helm-ff-symlink ((t (:inherit font-lock-comment-face :extend t :foreground "#842879"))))))

(use-package doom-modeline
  :hook ((after-init . doom-modeline-mode)))

;; Smooth scrolling
(use-package good-scroll
  :hook ((after-init . good-scroll-mode))
  :init
  (setq good-scroll-step 180))

(provide 'init-overhaul)
