;;; init-fsharp.el -- The OCaml-influcened multi-paradigm language

(use-package fsharp-mode
  :hook
  (fsharp-mode . lsp))

(provide 'init-fsharp)
