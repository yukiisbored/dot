;;; init-org.el --- Setup org-mode

(use-package org
  :hook
  ((org-mode . (lambda () (hl-line-mode nil)))
   (org-mode . auto-fill-mode))
  :init
  (setq initial-major-mode                  'org-mode
        org-startup-indented                t
        org-pretty-entities                 t
        org-hide-emphasis-markers           t
        org-fontify-whole-heading-line      t
        org-fontify-done-headline           t
        org-fontify-quote-and-verse-blocks  t
        org-ellipsis                        "  "
        org-src-tab-acts-natively           t
        org-src-fontify-natively            t
        org-latex-to-pdf-process            `("xelatex -interaction nonstopmode %f"
                                              "xelatex -interaction nonstopmode %f")
        org-latex-listings                  'minted
        org-confirm-babel-evaluate          nil
        org-export-with-smart-quotes        t
        org-startup-with-inline-images      t
        org-src-window-setup                'current-window)
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
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  (font-lock-add-keywords 'org-mode
                            '(("^ *\\(-\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (let* ((variable-tuple (cond ((x-list-fonts "Fira Sans")    '(:font   "Fira Sans"))
                               ((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
                               (nil (warn "Cannot find a Sans Serif Font. Install Fira Sans."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces 'user
                            `(org-level-8 ((t (,@headline ,@variable-tuple))))
                            `(org-level-7 ((t (,@headline ,@variable-tuple))))
                            `(org-level-6 ((t (,@headline ,@variable-tuple))))
                            `(org-level-5 ((t (,@headline ,@variable-tuple))))
                            `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                            `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                            `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                            `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                            `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil)))))))

(use-package ein
  :after org
  :if (executable-find "jupyter")
  :init (cl-pushnew '(ein . t) load-language-list))

(use-package graphviz-dot-mode
  :after org
  :config
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)))

(use-package org-bullets
  :hook ((org-mode . org-bullets-mode))
  :init
  (setq org-bullets-bullet-list '(" ")))

(use-package org-sidebar)

(provide 'init-org)
