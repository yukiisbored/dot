;;; init-ui.el --- Init for UI Customization and related packages
;; Author: Muhammad Kaisar Arkhan (yuki_is_bored)

;;; Commentary:

;;; Code:

(require 'req-package)

;; Cleaner UI

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Font

(add-to-list 'default-frame-alist '(font . "Go Mono-10"))
(set-face-attribute 'default t :font "Go Mono-10")

;; Pane movement with arrow keys

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(global-linum-mode 1)
(global-hl-line-mode 1)

(column-number-mode 1)

(req-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(req-package powerline
  :config
  (powerline-default-theme))

(req-package mode-icons
  :config
  (mode-icons-mode 1))

(req-package tabbar)

(req-package tabbar-ruler
  :require tabbar projectile
  :bind
  ("C-q" . tabbar-ruler-move)
  :init
  (setq tabbar-ruler-global-tabbar t)
  (setq tabbar-ruler-popup-menu t)
  (setq tabbar-ruler-popup-toolbar t)
  (require 'tabbar-ruler)
  :config
  (setq tabbar-buffer-groups-function
        'tabbar-ruler-projectile-tabbar-buffer-groups))

(req-package monokai-theme
  :config
  (load-theme 'monokai t))

(req-package which-key
  :config
  (which-key-setup-minibuffer)
  (which-key-mode 1))

(provide 'init-ui)

;;; init-ui.el ends here
