;;; init-ui.el --- Emacs UI overhaul because the default sucks(tm)

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
  :bind
  ("C-c C-r" . ivy-resume)
  :init
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t)
  (add-hook 'after-init-hook
	    (lambda ()
	      (ivy-mode t))))

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


;; File tree
(use-package neotree
  :bind
  ("C-x f" . neotree-toggle)
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  (add-hook 'after-init-hook #'neotree-toggle))

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
  (setq dashboard-banner-logo-title "Hi Yuki, Welcome to GNU Emacs.")
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-items `((recents . 5)
			  (projects . 5)
			  (registers . 5)))
  (dashboard-setup-startup-hook))

;; All the Icons
(use-package all-the-icons
  :ensure all-the-icons-dired
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; Moe theme
(use-package moe-theme
  :init
  (add-hook 'after-init-hook
            (lambda ()
              (require 'moe-theme)
              (moe-theme-set-color 'red)
              (moe-light))))

(provide 'init-ui)
