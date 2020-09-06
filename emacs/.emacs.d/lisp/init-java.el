;;; init-java.el --- Setup Java mode

(use-package lsp-java
  :hook
  (java-mode . lsp))

(provide 'init-java)
