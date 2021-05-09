;;; init-org.el --- Setup org-mode

(use-package org
  :straight org-plus-contrib
  :init
  (setq org-src-tab-acts-natively      t
        org-src-fontify-natively       t
        org-latex-to-pdf-process       `("xelatex -interaction nonstopmode %f"
                                         "xelatex -interaction nonstopmode %f")
        org-latex-listings             'minted
        org-confirm-babel-evaluate     nil
        org-export-with-smart-quotes   t
        org-ellipsis                   "⤵"
        initial-major-mode             'org-mode
        org-startup-with-inline-images t
        org-src-window-setup           'current-window)
  :config
  (defvar load-language-list `((emacs-lisp . t)
                               (perl       . t)
                               (python     . t)
                               (ruby       . t)
                               (js         . t)
                               (css        . t)
                               (sass       . t)
                               (C          . t)
                               (java       . t)
                               (plantuml   . t)))
  (org-babel-do-load-languages 'org-babel-load-languages load-language-list)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (eval-after-load 'ox '(require 'ox-koma-letter))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

(use-package ein
  :after org
  :if (executable-find "jupyter")
  :init (cl-pushnew '(ein . t) load-language-list))

(use-package graphviz-dot-mode
  :after org
  :config
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)))

(provide 'init-org)
