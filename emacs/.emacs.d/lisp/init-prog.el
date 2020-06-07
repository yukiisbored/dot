;;; init-prog.el --- Essential things that makes programming better

;; Makes it clear where the fuck you're on(tm)
(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'fill)
  (add-hook 'prog-mode-hook #'highlight-indent-guides-mode))

;; A Better Git interface than git(1)
(use-package magit
  :bind
  ("C-x g" . magit-status))

;; Automatic whitespace cleanup
(use-package whitespace-cleanup-mode
  :init
  (add-hook 'after-init-hook
            (lambda ()
              (global-whitespace-cleanup-mode t))))

;; Persistent Undo
(use-package undo-tree
  :config
  (unless (file-directory-p "~/.emacs.d/undo")
    (make-directory "~/.emacs.d/undo"))
  (setq undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo")))
  (setq undo-tree-auto-save-history t))

;; Highlight TODOs
(use-package hl-todo
  :init
  (add-hook 'after-init-hook 'global-hl-todo-mode))

;; Highlight version control diffs
(use-package diff-hl
  :hook
  (dired-mode . diff-hl-dired-mode)
  :init
  (add-hook 'after-init-hook 'global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; Show colors for rgb(rr,gg,bb) and #rrggbb
(use-package rainbow-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-mode))

;; IMenu Anywhere
(use-package imenu-anywhere
  :bind
  ("C-c ." . imenu-anywhere))

;; Multiple cursors
(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("C-c c" . mc/edit-lines))
  :config
  (define-key mc/keymap (kbd "<return>") nil))

(provide 'init-prog)
