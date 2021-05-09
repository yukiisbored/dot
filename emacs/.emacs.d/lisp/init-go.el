;;; init-go.el --- Setup go mode

(use-package go-mode
  :mode "\\.go\\'"
  :hook ((before-save . gofmt-before-save)))

(provide 'init-go)
