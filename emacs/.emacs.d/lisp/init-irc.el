;;; init-irc --- Setup circe

(setq irc-setup-el (expand-file-name "irc-setup.el" user-emacs-directory))

(when (file-exists-p irc-setup-el)
  (load-file irc-setup-el))

(provide 'init-irc)
