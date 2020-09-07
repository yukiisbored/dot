;;; init-python.el --- Setup Python mode

;; Use Python 3 ffs
(eval-and-compile
  (if (executable-find "python3")
      (setq python-shell-interpreter "python3")
    (if (executable-find "python3.7")
        (setq python-shell-interpreter "python3.7")
      (message "Failed to find Python 3 intepreter"))))

(eval-and-compile
  (setq flycheck-python-pycompile-executable python-shell-interpreter)
  (setq flycheck-python-flake8-executable python-shell-interpreter)
  (setq flycheck-python-pylint-executable python-shell-interpreter)
  (setq org-babel-python-command python-shell-interpreter)
  (setq python-shell-completion-native-enable 'nil))

(use-package poetry)
(use-package live-py-mode)

(use-package lsp-python-ms
  :if (or (string-equal system-type "gnu/linux")
          (string-equal system-type "windows-nt")
          (string-equal system-type "darwin"))
  :init
  (setq lsp-python-ms-auto-install-server t))

(add-hook 'python-mode-hook 'lsp)

(provide 'init-python)
