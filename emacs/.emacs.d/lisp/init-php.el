;;; init-php.el --- Setup PHP mode

(use-package php-mode
  :hook
  (php-mode . lsp))

(provide 'init-php)
