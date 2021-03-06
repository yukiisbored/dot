;;; init-lsp.el --- Language server support for Emacs.

(use-package lsp-mode
  :after direnv
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . lsp-lens-mode)
  :init
  (setq lsp-enable-file-watchers nil
        lsp-keymap-prefix "C-c l")
  (advice-add 'lsp :before 'direnv-update-environment)) ;; Hack to get direnv running before lsp

(use-package lsp-ui
  :after lsp-mode)

(use-package lsp-ivy
  :after lsp-mode ivy)

(use-package lsp-treemacs
  :after lsp-mode treemacs)

(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(provide 'init-lsp)
