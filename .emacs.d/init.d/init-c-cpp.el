;;; init-c-cpp.el --- Init for C/C++ packages
;; Author: Muhammad Kaisar Arkhan (yuki_is_bored)

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package irony
  :require auto-complete
  :init
  (defun my-irony-hook ()
    (add-to-list 'ac-sources 'ac-source-irony))
  (add-hook 'irony-mode-hook 'my-irony-hook)
  :config
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode))

(req-package flycheck-irony
  :require flycheck)

(provide 'init-c-cpp)

;;; init-c-cpp.el ends here
