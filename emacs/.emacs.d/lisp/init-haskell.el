;;; init-haskell.el --- Setup environment for haskell

(use-package haskell-mode
  :hook
  (haskell-mode . interactive-haskell-mode))

(use-package lsp-haskell
  :hook
  (haskell-mode . lsp)
  (haskell-literate-mode . lsp))

(provide 'init-haskell)
