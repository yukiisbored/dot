;;; init-lsp.el --- Language server support for Emacs.

(use-package lsp-mode
  :after direnv
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (lsp-mode . lsp-lens-mode)

   (python-mode     . lsp)
   (csharp-mode     . lsp)
   (php-mode        . lsp)
   (go-mode         . lsp)
   (elixir-mode     . lsp)
   (java-mode       . lsp)
   (typescript-mode . lsp)
   (nix-mode        . lsp)
   (scala-mode      . lsp)
   (haxe-mode       . lsp)
   (rust-mode       . lsp)
   (svelte-mode     . lsp)
   (dhall-mode      . lsp)
   (purescript-mode . lsp))
  :init
  (setq lsp-enable-file-watchers nil
        lsp-keymap-prefix "C-c l")
  (advice-add 'lsp :before 'direnv-update-environment)) ;; Hack to get direnv running before lsp

(use-package lsp-ui
  :after lsp-mode)

(use-package lsp-ivy
  :after lsp-mode ivy)

(use-package dap-mode
  :hook
  ((lsp-mode . dap-mode)
   (lsp-mode . dap-ui-mode)))

(use-package lsp-haskell
  :hook
  ((haskell-mode . lsp)
   (haskell-literate-mode . lsp)))

(provide 'init-lsp)
