;;; init-web.el --- Setup web modes

(use-package web-mode
  :mode (("\\.phtml\\'"     . web-mode)
         ("\\.twig\\'"      . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'"   . web-mode)
         ("\\.as[cp]x\\'"   . web-mode)
         ("\\.erb\\'"       . web-mode)
         ("\\.mustache\\'"  . web-mode)
         ("\\.djhtml\\'"    . web-mode)
         ("\\.html?\\'"     . web-mode)
         ("\\.j?j2\\'"      . web-mode)
         ("\\.vue\\'"       . web-mode)
         ("\\.svelte\\'"    . svelte-mode))
  :init
  (setq web-mode-engines-alist '(("django" . "\\.j?j2\\'")))
  (define-derived-mode svelte-mode web-mode "svelte-mode"))

(use-package pug-mode)

(provide 'init-web)
