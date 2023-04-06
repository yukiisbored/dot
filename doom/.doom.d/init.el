;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (company +childframe)
       (vertico +icons)

       :ui
       doom
       doom-dashboard
       doom-quit
       hl-todo
       indent-guides
       modeline
       nav-flash
       ophints
       (popup +defaults)
       vc-gutter
       vi-tilde-fringe
       (window-select +switch-window)
       workspaces
       zen
       deft
       (treemacs +lsp)

       :editor
       (evil +everywhere)
       format
       multiple-cursors
       rotate-text
       snippets
       word-wrap
       fold

       :emacs
       (dired +icons +ranger)
       electric
       (ibuffer +icons)
       (undo +tree)
       vc

       :term
       (:unless IS-WINDOWS vterm)

       :checkers
       (syntax +childframe)
       (spell +aspell)
       grammar

       :tools
       editorconfig
       (eval +overlay)
       lookup
       (lsp +peek)
       magit
       pdf
       rgb
       tree-sitter
       direnv

       :os
       (:if IS-MAC macos)
       (tty +osc)

       :lang
       (cc +lsp +tree-sitter)
       (dart +flutter +lsp)
       emacs-lisp
       (haskell +lsp)
       (json +tree-sitter)
       (javascript +lsp +tree-sitter)
       markdown
       (nix +tree-sitter)
       (python +lsp +pyright +tree-sitter)
       (rust +lsp +tree-sitter)
       (sh +tree-sitter)
       (web +lsp +tree-sitter)
       yaml
       org

       :config
       (default +bindings +smartparens))
