;;; init-flychceck.el --- Setup flycheck

(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package flycheck-inline
  :init
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook 'flycheck-inline-mode)))

(provide 'init-flycheck)
