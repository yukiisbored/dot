;;; init-shell-path.el --- Grab environment variables from shell

(use-package exec-path-from-shell
  :if window-system
  :init
  (setq-default exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (add-hook 'after-init-hook
            #'exec-path-from-shell-initialize))

(provide 'init-shell-path)
