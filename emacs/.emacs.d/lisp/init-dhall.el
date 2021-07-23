;;; init-dhall.el --- Dhall

(use-package dhall-mode
  :config
  (setq dhall-format-arguments (\` ("--ascii"))
        dhall-use-header-line nil))

(provide 'init-dhall)
