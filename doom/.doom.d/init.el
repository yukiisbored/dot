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
       (emoji +github +unicode)
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
       coq
       data
       (dart +flutter +lsp)
       dhall
       (elixir +lsp +tree-sitter)
       (elm +tree-sitter)
       emacs-lisp
       (erlang +lsp)
       (go +lsp +tree-sitter)
       (gdscript +lsp)
       (haskell +lsp)
       (json +tree-sitter)
       (javascript +lsp +tree-sitter)
       (kotlin +lsp)
       markdown
       (nix +tree-sitter)
       (org +roam2 +pretty)
       (php +lsp +tree-sitter)
       (purescript +lsp)
       (python +lsp +pyright +tree-sitter)
       (rust +lsp +tree-sitter)
       (sh +tree-sitter)
       (web +lsp +tree-sitter)
       yaml
       (zig +lsp +tree-sitter)

       :config
       (default +bindings +smartparens))
