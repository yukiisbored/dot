;;; init-rust.el --- Setup rust mode

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package cargo
  :hook
  (rust-mode . cargo-minor-mode))

(provide 'init-rust)
