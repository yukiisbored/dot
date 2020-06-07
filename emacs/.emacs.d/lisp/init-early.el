;; init-early.el -- Early customizations to Emacs

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerr)

(setq package-enable-at-startup nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq frame-inhibit-implied-resize t)
 
(provide 'init-early)

