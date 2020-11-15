;;; init-elixir.el --- Setup Elixir

(use-package elixir-mode
  :hook
  (elixir-mode . lsp))

(provide 'init-elixir)
