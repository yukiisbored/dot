;;; init-eglot.el --- EGlot

(use-package eglot
  :hook ((python-mode     . eglot-ensure)
         (csharp-mode     . eglot-ensure)
         (php-mode        . eglot-ensure)
         (go-mode         . eglot-ensure)
         (elixir-mode     . eglot-ensure)
         (java-mode       . eglot-ensure)
         (haskell-mode    . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (nix-mode        . eglot-ensure)
         (scala-mode      . eglot-ensure)
         (haxe-mode       . eglot-ensure)
         (haxe-mode       . yuki/haxe-workspace-config)
         (rust-mode       . eglot-ensure)
         (svelte-mode     . eglot-ensure)
         (dhall-mode      . eglot-ensure)
         (purescript-mode . eglot-ensure))
  :bind (:map eglot-mode-map
         ("C-c e r" . eglot-rename)
         ("C-c e f" . eglot-format)
         ("C-c e h" . eglot-help-at-point))
  :init
  (defun yuki/haxe-workspace-config ()
    (setq-local eglot-workspace-configuration
                `((:haxe-language-server . (:haxe.executable "haxe")))))
  :config
  ;; C#
  (add-to-list 'eglot-server-programs
               `(csharp-mode . ("omnishare" "-lsp")))

  ;; Haxe
  (add-to-list 'eglot-server-programs
               `(haxe-mode . (eglot-haxe-language-server "haxe-language-server")))

  (defclass eglot-haxe-language-server (eglot-lsp-server) ()
    :documentation "A custom class for Haxe Language Server")

  (cl-defmethod eglot-initialization-options ((server eglot-haxe-language-server))
    "Passes the build.hxml file as displayArguments"
    (let* ((root (car (project-roots (eglot--project server))))
           (build-hxml (expand-file-name "build.hxml" root))
           (compile-hxml (expand-file-name "compile.hxml" root))
           (hxml-file (if (file-exists-p compile-hxml) compile-hxml build-hxml)))
      (list :displayArguments hxml-file)))

  ;; Svelte
  (add-to-list 'eglot-server-programs
               `(svelte-mode . ("svelteserver" "--stdio")))

  ;; Dhall
  (add-to-list 'eglot-server-programs
               `(dhall-mode . ("dhall-lsp-server")))

  ;; Purescript
  (add-to-list 'eglot-server-programs
               `(purescript-mode . ("purescript-language-server" "--stdio"))))

(provide 'init-eglot)
