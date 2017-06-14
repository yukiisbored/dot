;;; init-ruby.el --- Init for Ruby packages
;; Author: Muhammad Kaisar Arkhan (yuki_is_bored)

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package robe
  :require company
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  (push 'company-robe company-backends))

(req-package rubocop
  :config
  (add-hook 'ruby-mode-hook 'rubocop-mode))

(provide 'init-ruby)

;;; init-ruby.el ends here
