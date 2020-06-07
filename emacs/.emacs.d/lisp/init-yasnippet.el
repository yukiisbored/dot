;;; init-yasnippet.el --- Setup templating system

(use-package yasnippet
  :ensure yasnippet-snippets
  :init
  (setq yas-snippet-dirs `("~/.emacs.d/snippets"))
  (add-hook 'after-init-hook 'yas-global-mode))

(provide 'init-yasnippet)
