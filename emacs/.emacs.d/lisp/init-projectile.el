;;; init-projectile.el --- Setup projectile

(use-package projectile
  :ensure projectile-git-autofetch
  :init
  (setq projectile-completion-system 'ivy)
  (add-hook 'after-init-hook
            (lambda ()
              (projectile-mode t))))

(provide 'init-projectile)
