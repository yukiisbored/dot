;;; init-go.el --- Setup go mode

(use-package go-mode
  :mode "\\.go\\'"
  :init
  (add-hook 'before-save-hook 'gofmt-before-save))

(provide 'init-go)
