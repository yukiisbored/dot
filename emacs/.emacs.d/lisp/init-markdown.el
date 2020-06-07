;;; init-markdown.el --- Setup markdown mode

(use-package markdown-mode
  :mode "\\.md\\'"
  :init
  (setq markdown-command "multimarkdown"))

(provide 'init-markdown)
