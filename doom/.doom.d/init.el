;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (company +childframe)
       (vertico +childframe +icons)

       :ui
       doom
       doom-dashboard
       doom-quit
       hl-todo
       indent-guides
       modeline
       nav-flash
       ophints
       (popup +all +defaults)
       vc-gutter
       vi-tilde-fringe
       (window-select +switch-window)
       workspaces
       zen

       :editor
       (evil +everywhere)
       format
       multiple-cursors
       rotate-text
       snippets
       word-wrap

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
       (spell +hunspell +everywhere)

       :tools
       editorconfig
       (eval +overlay)
       lookup
       (lsp +peek)
       magit
       pdf
       rgb
       tree-sitter

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
       (javascript +tree-sitter)
       (kotlin +lsp)
       markdown
       (nix +tree-sitter)
       (ocaml +lsp +tree-sitter)
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
