;;; init-eglot.el --- EGlot

(use-package eglot
  :hook
  ((python-mode . eglot-ensure)
   (csharp-mode . eglot-ensure))
  :config
  (define-key eglot-mode-map (kbd "C-c e r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c e f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c e h") 'eglot-help-at-point)
  (add-to-list 'eglot-server-programs
               `(csharp-mode . ("omnishare" "-lsp"))))

(provide 'init-eglot)
