;;; init-go.el --- Init for Go packages
;; Author: Muhammad Kaisar Arkhan (yuki_is_bored)

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package go-mode
  :ensure t)

(req-package go=autocomplete
  :require auto-complete)

(provide 'init-go)

;;; init-go.el ends here
