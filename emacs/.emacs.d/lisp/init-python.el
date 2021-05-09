;;; init-python.el --- Setup Python mode

;; Use Python 3 ffs
(eval-and-compile
  (if (executable-find "python3.7")
      (setq python-shell-interpreter "python3.7")
    (if (executable-find "python3.8")
        (setq python-shell-interpreter "python3.8")
      (setq python-shell-interpreter "python3"))))

(eval-and-compile
  (setq flycheck-python-pycompile-executable python-shell-interpreter)
  (setq flycheck-python-flake8-executable python-shell-interpreter)
  (setq flycheck-python-pylint-executable python-shell-interpreter)
  (setq org-babel-python-command python-shell-interpreter)
  (setq python-shell-completion-native-enable 'nil))

(use-package poetry)
(use-package live-py-mode)

(use-package pyvenv)
(use-package pipenv
  :hook ((python-mode . pipenv-mode))
  :init
  (setq pipenv-projectile-after-switch-function
        'pipenv-projectile-after-switch-extended))

(use-package elpy
  :init
  (elpy-enable))

(provide 'init-python)
