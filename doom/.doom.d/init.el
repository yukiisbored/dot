;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :completion
       company                         ; the ultimate code completion backend
       (ivy +flx +prescient +icons)    ; /The/ search engine for love and life

       :ui
       doom                            ; what makes DOOM look the way it does
       doom-dashboard                  ; a nifty splash screen for Emacs
       doom-quit                       ; DOOM quit-message prompts when you quit Emacs
       hl-todo                         ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       indent-guides                   ; highlighted indent columns
       ophints                         ; highlight the region an operation acts on
       (popup +defaults)               ; tame sudden yet inevitable temporary windows
       treemacs                        ; a project drawer, like neotree but cooler
       vc-gutter                       ; vcs diff in the fringe
       vi-tilde-fringe                 ; fringe tildes to mark beyond EOB
       (window-select +switch-window)  ; visually switch windows
       workspaces                      ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere)              ; come to the dark side, we have cookies
       file-templates                  ; auto-snippets for empty files
       fold                            ; (nigh) universal code folding
       multiple-cursors                ; editing in many places at once
       snippets                        ; my elves. They type so I don't have to
       word-wrap                       ; soft wrapping with language-aware indent

       :emacs
       (dired +icons)                  ; making dired pretty [functional]
       electric                        ; smarter, keyword-based electric-indent
       (ibuffer +icons)                ; interactive buffer management
       undo                            ; persistent, smarter undo for your inevitable mistakes
       vc                              ; version-control and Emacs, sitting in a tree

       :term
       vterm                           ; the best terminal emulation in Emacs

       :checkers
       syntax                          ; tasing you for every semicolon you forget

       :tools
       editorconfig                    ; let someone else argue about tabs vs spaces
       (eval +overlay)                 ; run code, run (also, repls)
       lookup                          ; navigate your code and its documentation
       lsp                             ; M-x vscode
       magit                           ; a git porcelain for Emacs
       rgb                             ; creating color strings

       :os
       (:if IS-MAC macos)              ; improve compatibility with macOS
       (tty +osc)                      ; improve the terminal Emacs experience

       :lang
       (cc +lsp)                       ; C > C++ == 1
       coq                             ; proofs-as-programs
       data                            ; config/data formats
       (dart +flutter +lsp)            ; paint ui and not much else
       dhall
       (elixir +lsp)                   ; erlang done right
       elm                             ; care for a cup of TEA?
       emacs-lisp                      ; drown in parentheses
       erlang                          ; an elegant language for a more civilized age
       fsharp                          ; ML stands for Microsoft's Language
       fstar                           ; (dependent) types and (monadic) effects and Z3
       (go +lsp)                       ; the hipster dialect
       (haskell +lsp)                  ; a language that's lazier than I am
       json                            ; At least it ain't XML
       (java +lsp)                     ; the poster child for carpal tunnel syndrome
       javascript                      ; all(hope(abandon(ye(who(enter(here))))))
       (kotlin +lsp)                   ; a better, slicker Java(Script)
       latex                           ; writing papers in Emacs has never been so fun
       lua                             ; one-based indices? one-based indices
       markdown                        ; writing docs for people to ignore
       nim                             ; python + lisp at the speed of c
       nix                             ; I hereby declare "nix geht mehr!"
       ocaml                           ; an objective camel
       (org +roam2)                    ; organize your plain life in plain text
       php                             ; perl's insecure younger brother
       purescript                      ; javascript, but functional
       (python +lsp +pyright)          ; beautiful is better than ugly
       qt                              ; the 'cutest' gui framework ever
       (rust +lsp)                     ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       sh                              ; she sells {ba,z,fi}sh shells on the C xor
       swift                           ; who asked for emoji variables?
       (web +lsp)                      ; the tubes
       yaml                            ; JSON, but readable
       zig                             ; C, but simpler

       :config
       (default +bindings +smartparens))
