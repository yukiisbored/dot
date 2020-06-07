;;; init-org.el --- Setup org-mode

(use-package org
  :ensure org-plus-contrib
  :init
  (setq org-src-tab-acts-natively t)
  (setq org-src-fontify-natively t)
  (setq org-latex-to-pdf-process `("xelatex -interaction nonstopmode %f"
                                   "xelatex -interaction nonstopmode %f"))
  (setq org-latex-listings 'minted)
  (setq org-confirm-babel-evaluate nil)
  (setq org-export-with-smart-quotes t)
  (setq org-ellipsis "⤵")
  (setq initial-major-mode 'org-mode)
  (setq org-startup-with-inline-images t)
  (setq org-src-window-setup 'current-window)
  :config
  (defvar load-language-list `((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (plantuml . t)))

  (use-package ob-ipython
    :if (executable-find "jupyter")
    :init (cl-pushnew '(ipython . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages load-language-list)

  (use-package org-bullets
    :hook (org-mode . org-bullets-mode))

  (use-package graphviz-dot-mode
    :config
    (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)))

  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (eval-after-load 'ox '(require 'ox-koma-letter))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

(provide 'init-org)
