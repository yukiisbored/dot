;;; init-clojure.el --- Setup Clojure on Emacs

(use-package clojure-mode
  :hook
  (clojure-mode . lsp)
  (clojurescript-mode . lsp)
  (clojurec-mode . lsp))

(use-package cider)

(provide 'init-clojure)
