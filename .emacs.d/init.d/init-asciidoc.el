;;; init-asciidoc.el --- Init for Asciidoc packages
;; Author: Muhammad Kaisar Arkhan (yuki_is_bored)

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package adoc-mode
  :mode
  (("\\.adoc\\'" . adoc-mode)
   ("\\.asciidoc\\'" . adoc-mode)))

(provide 'init-asciidoc)

;;; init-asciidoc.el ends here
