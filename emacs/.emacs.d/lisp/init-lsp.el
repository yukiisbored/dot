;;; init-lsp.el --- Language server support for Emacs.

(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . lsp-lens-mode))

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
