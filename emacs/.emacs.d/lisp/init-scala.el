;;; init-scala.el -- Provide Scala support for Emacs

(use-package scala-mode
  :interpreter
  (("scala" . scala-mode)))

(use-package sbt-mode
  :init
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package lsp-metals
  :hook
  (scala-mode . lsp))

(provide 'init-scala)
