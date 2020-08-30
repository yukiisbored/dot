;;; init-mail.el --- Setup mu4e

(if (file-directory-p "/usr/local/share/emacs/site-lisp/mu4e")
    (setq mu4e-directory "/usr/local/share/emacs/site-lisp/mu4e")
  (setq mu4e-directory "/usr/share/emacs/site-lisp/mu4e"))

(setq email-setup-el (expand-file-name "email-setup.el" user-emacs-directory))

(when (file-directory-p mu4e-directory)
  (add-to-list 'load-path mu4e-directory)
  (require 'mu4e)
  (when (file-exists-p email-setup-el)
    (load-file email-setup-el)))

(provide 'init-mail)
