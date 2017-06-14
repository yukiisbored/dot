;;; init-editing.el --- Init for general text editing packages
;; Author: Muhammad Kaisar Arkhan (yuki_is_bored)

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode 1))

(req-package fill-column-indicator
  :init
  (setq fci-rule-column 80))

(req-package undo-tree
  :config
  (global-undo-tree-mode 1))

(provide 'init-editing)

;;; init-editing.el ends here
