;;; init-rust.el --- Setup rust mode

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package flycheck-rust
  :init
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)))

(use-package racer
  :init
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  :config
  (define-key rust-mode-map (kbd "TAB") 'company-indent-or-complete-common))

(use-package cargo
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(provide 'init-rust)
