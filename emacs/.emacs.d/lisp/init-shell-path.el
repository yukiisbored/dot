;;; init-shell-path.el --- Grab environment variables from shell

(require 'use-package)

(use-package exec-path-from-shell
  :if window-system
  :hook ((after-init . exec-path-from-shell-initialize))
  :init
  (setq-default exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "NIX_PATH")))

(provide 'init-shell-path)
