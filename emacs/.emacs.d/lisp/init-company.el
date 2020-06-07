;;; init-company.el --- Setup company

(eval-and-compile
  (if (executable-find "hunspell")
    (setq ispell-program-name "hunspell")
  (if (executable-find "ispell")
      (setq ispell-program-name "ispell")
    (message "Please install hunspell/ispell for spell checking."))))

(use-package company
  :bind
  ("C-c y" . company-yasnippet)
  :init
  (setq company-tooltip-align-annotations t)
  (add-hook 'after-init-hook
            (lambda ()
              (global-company-mode t)))
  (defun yuki/spelling-setup ()
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends 'company-ispell))
  (add-hook 'text-mode-hook-setup #'yuki/spelling-setup)
  (add-hook 'org-mode-hook-setup #'yuki/spelling-setup)
  (add-hook 'markdown-mode-hook-setup #'yuki/spelling-setup))

(provide 'init-company)
