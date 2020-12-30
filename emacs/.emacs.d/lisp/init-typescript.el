;;; init-typescript.el -- Setup environment for typescript

(use-package typescript-mode
  :hook
  (typescript-mode . lsp))

(provide 'init-typescript)
