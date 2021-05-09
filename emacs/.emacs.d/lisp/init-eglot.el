;;; init-eglot.el --- EGlot

(use-package eglot
  :hook ((python-mode     . eglot-ensure)
         (csharp-mode     . eglot-ensure)
         (php-mode        . eglot-ensure)
         (go-mode         . eglot-ensure)
         (elixir-mode     . eglot-ensure)
         (java-mode       . eglot-ensure)
         (haskell-mode    . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (nix-mode        . eglot-ensure)
         (scala-mode      . eglot-ensure))
  :bind (:map eglot-mode-map
         ("C-c e r" . eglot-rename)
         ("C-c e f" . eglot-format)
         ("C-c e h" . eglot-help-at-point))
  :config
  (add-to-list 'eglot-server-programs
               `(csharp-mode . ("omnishare" "-lsp"))))

(provide 'init-eglot)
