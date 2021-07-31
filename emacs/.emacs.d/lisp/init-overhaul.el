;;; init-overhaul.el --- Make Emacs even more powerful

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
    (set-frame-font "Fira Code-12" nil t)
    (modify-frame-parameters frame
                             `((min-height . 1)
                               (height     . 45)
	                       (min-width  . 1)
                               (width      . 81)
                               (vertical-scroll-bars . nil)
                               (internal-border-width . 24)
                               (left-fringe    . 1)
                               (right-fringe   . 1)
                               (tool-bar-lines . 0)
                               (menu-bar-lines . 0)))))

(add-hook 'after-make-frame-functions 'yuki/frame-mods)
(add-hook 'after-init-hook
          (lambda ()
            (yuki/frame-mods (selected-frame))))

;; Underline should be on the bottomline
(setq x-underline-at-descent-line t)

;; Vertical window divider
(setq window-divider-default-right-width 24
      window-divider-default-places      'right-only)

(window-divider-mode 1)

;; Make Emacs UI look more bearable
(setq widget-image-enable nil)

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
                                     (expand-file-name "dashboard_banner.png" user-emacs-directory)
                                   (expand-file-name "dashboard_banner.txt" user-emacs-directory))
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

;; Neotree
(use-package neotree
  :bind (("C-x t" . neotree-toggle))
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

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

;; Emojis
(use-package emojify
  :hook (after-init . global-emojify-mode))

;; Auto formatting
(use-package format-all
  :hook (prog-mode . format-all-mode))

;; Doom Emacs Theme
(use-package doom-themes
  :if window-system
  :hook ((after-init . (lambda () (load-theme 'doom-opera-light t)))
         (after-init . doom-themes-visual-bell-config)
         (after-init . doom-themes-treemacs-config)
         (after-init . doom-themes-org-config))
  :init
  ;; Increase contrast for some stuff
  ;; Some people really need to read W3C WCAG
  (setq doom-opera-light-brighter-comments t)

  ;; Invisible window-divider
  (custom-set-faces
   '(window-divider ((t (:foreground "#fafafa" :inherit (vertical-border)))))))

(use-package doom-modeline
  :hook ((after-init . doom-modeline-mode))
  :init
  (setq doom-modeline-height 48
        doom-modeline-bar-width 1)

  ;; Invisible modelines
  (custom-set-faces
   '(mode-line-inactive ((t (:box nil :foreground "#9e9e9e" :background "#fafafa"))))
   '(mode-line   ((t (:box nil :background "#fafafa"))))))

(use-package ligature
  :hook ((after-init . global-ligature-mode))
  :straight (el-patch :type git
                      :host github :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%")))

(provide 'init-overhaul)
