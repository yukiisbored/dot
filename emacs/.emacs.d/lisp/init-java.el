;;; init-java.el --- Setup Java mode

(use-package lsp-java
  :hook
  (java-mode . lsp)
  :init
  (setq lombok-jar-path (expand-file-name "lombok.jar" user-emacs-directory))
  :config
  (when (file-exists-p lombok-jar-path)
    (add-to-list 'lsp-java-vmargs (concat "-javaagent:" lombok-jar-path))))

(provide 'init-java)
