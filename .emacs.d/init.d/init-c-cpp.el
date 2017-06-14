;;; init-c-cpp.el --- Init for C/C++ packages
;; Author: Muhammad Kaisar Arkhan (yuki_is_bored)

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package irony
  :config
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode))

(req-package flycheck-irony
  :require flycheck)

(req-package company-irony
  :require company)

(provide 'init-c-cpp)

;;; init-c-cpp.el ends here
