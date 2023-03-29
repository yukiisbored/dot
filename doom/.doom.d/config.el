;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(require 'lsp)

(setq user-full-name    "Yuki"
      user-mail-address "hi@yukiisbo.red")

(setq doom-font  (font-spec :family "Julia Mono" :size 14)
      doom-theme 'modus-operandi)

(setq display-line-numbers-type 'relative
      warning-minimum-level :error)

(setq org-roam-directory "~/roam")

(add-to-list 'auto-mode-alist '("\\.mdx" . markdown-mode))

(defun yuki/lsp-set-priority (server priority)
  (setf (lsp--client-priority (gethash server lsp-clients)) priority))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map evil-insert-state-map
              ("M-<right>" . copilot-accept-completion-by-line)
              ("M-<return>" . copilot-accept-completion)
              ("M-<tab>" . copilot-next-completion)))

(use-package! wakatime
  :hook (after-init . global-wakatime-mode))

(use-package! prisma-mode
  :after lsp
  :init
  (add-hook 'prisma-mode-hook #'lsp-deferred)
  :config
  (yuki/lsp-set-priority 'prismals -2))
