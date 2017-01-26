;; Yuki's .emacs
;; Now with 100s less LOC!

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Emacs and Built-in Settings
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(global-linum-mode 1)
(global-hl-line-mode 1)

(add-to-list 'default-frame-alist '(font . "Hack-10"))
(set-face-attribute 'default t :font "Hack-10")

(column-number-mode 1)

(setq make-backup-files nil)

(setq-default indent-tabs-mode nil)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Packages setup

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)))

(use-package tabbar
  :ensure t
  :config (tabbar-mode 1))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode 1))

(use-package projectile
  :ensure t
  :config (projectile-mode 1))

(use-package project-explorer
  :ensure t
  :bind ("M-r" . project-explorer-toggle))

(use-package find-file-in-project
  :ensure t
  :bind ("M-q" . find-file-in-project))

(use-package indent-guide
  :ensure t
  :config (indent-guide-global-mode 1))

(use-package whitespace-cleanup-mode
  :ensure t
  :config (global-whitespace-cleanup-mode 1))

(use-package company
  :ensure t
  :config (global-company-mode 1))

(use-package company-quickhelp
  :ensure t
  :config (company-quickhelp-mode 1))

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode 1))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode))

(use-package company-irony
  :ensure t)

(use-package flycheck-irony
  :ensure t)

(use-package robe
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  (eval-after-load 'company
    '(push 'company-robe company-backends)))

(use-package rubocop
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'rubocop-mode))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package go-mode
  :ensure t)

(use-package company-go
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-setup-minibuffer)
  (which-key-mode 1))

(use-package magit
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package rainbow-mode
  :ensure t)

(use-package adoc-mode
  :ensure t)

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

(use-package mode-icons
  :ensure t
  :config
  (mode-icons-mode 1))

(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-column 80))

(use-package dockerfile-mode
  :ensure t)

(use-package gradle-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package coffee-mode
  :ensure t)

(use-package origami
  :ensure t
  :bind ("C-`" . origami-toggle-node)
  :config
  (global-origami-mode 1))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (gradle-mode whitespace-cleanup-mode which-key web-mode use-package undo-tree tabbar smex rubocop robe rainbow-mode projectile project-explorer mode-icons material-theme markdown-mode magit indent-guide flycheck-irony find-file-in-project fill-column-indicator dockerfile-mode company-quickhelp company-irony company-go aggressive-indent adoc-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
