;;; init-programming.el --- Init for general programming packages
;; Author: Muhammad Kaisar Arkhan (yuki_is_bored)

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package projectile
  :config
  (projectile-mode 1))

(req-package auto-complete
  :config
  (ac-config-default))

(req-package flycheck
  :config
  (global-flycheck-mode 1))

(req-package magit
  :bind
  ("C-c C-z" . magit-status))

(req-package rainbow-mode)

(req-package origami
  :bind
  ("C-." . origami-toggle-node)
  :config
  (global-origami-mode 1))

(req-package smartparens
  :config
  (smartparens-global-mode 1))

(req-package highlight-parentheses
  :config
  (global-highlight-parentheses-mode 1))

(provide 'init-programming)

;;; init-programming.el ends here
