;;; init-programming.el --- Init for general programming packages
;; Author: Muhammad Kaisar Arkhan (yuki_is_bored)

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package projectile
  :config
  (projectile-mode 1))

(req-package indent-guide
  :config
  (indent-guide-global-mode 1))

(req-package company
  :config
  (global-company-mode 1))

(req-package company-quickhelp
  :require company
  :config
  (company-quickhelp-mode 1))

(req-package flycheck
  :config
  (global-flycheck-mode 1))

(req-package magit
  :bind
  ("C-c C-z" . magit-status))

(req-package rainbow-mode)

(req-package origami
  :bind
  ("C-`" . origami-toggle-node)
  :config
  (global-origami-mode 1))

(provide 'init-programming)

;;; init-programming.el ends here
