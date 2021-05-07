;;; init-haskell.el --- Setup environment for haskell

(use-package haskell-mode
  :hook
  (haskell-mode . interactive-haskell-mode))

(provide 'init-haskell)
