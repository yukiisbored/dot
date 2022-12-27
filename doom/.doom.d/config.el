;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name    "Yuki"
      user-mail-address "hi@yukiisbo.red")

(setq doom-font  (font-spec :family "Julia Mono" :size 14)
      doom-theme 'modus-operandi)

(setq display-line-numbers-type 'relative
      warning-minimum-level :error)

(setq org-roam-directory "~/roam")

(add-to-list 'auto-mode-alist '("\\.mdx" . markdown-mode))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map evil-insert-state-map
              ("M-<right>" . copilot-accept-completion-by-line)
              ("M-<return>" . copilot-accept-completion)
              ("M-<tab>" . copilot-next-completion))
  :init
  (setq copilot-node-executable (executable-find "node-16")))

(use-package! wakatime
  :hook (after-init . global-wakatime-mode))
