;;; init-haskell.el --- Init for Haskell packages
;; Author: Muhammad Kaisar Arkhan (yuki_is_bored)

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package haskell-mode)

(req-package intero
  :require haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(provide 'init-haskell)

;;; init-haskell.el ends here
