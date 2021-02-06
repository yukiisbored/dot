;;; init-exwm.el -- Emacs as a Window Manager

(use-package exwm
  :demand
  :init
  (require 'exwm-config)
  (require 'exwm-systemtray)

  (setq exwm-workspace-number 10

        exwm-input-global-keys `(([?\s-r] . exwm-reset)
                                 ([?\s-w] . exwm-workspace-switch)
                                 ,@(mapcar (lambda (i)
                                             `(,(kbd (format "s-%d" i)) .
                                               (lambda ()
                                                 (interactive)
                                                 (exwm-workspace-switch-create ,i))))
                                           (number-sequence 0 9)))

        exwm-input-simulation-keys '(;; movement
                                     ([?\C-b] . [left])
                                     ([?\M-b] . [C-left])
                                     ([?\C-f] . [right])
                                     ([?\M-f] . [C-right])
                                     ([?\C-p] . [up])
                                     ([?\C-n] . [down])
                                     ([?\C-a] . [home])
                                     ([?\C-e] . [end])
                                     ([?\M-v] . [prior])
                                     ([?\C-v] . [next])
                                     ([?\C-d] . [delete])
                                     ([?\C-k] . [S-end delete])
                                     ;; cut/paste.
                                     ([?\C-w] . [?\C-x])
                                     ([?\M-w] . [?\C-c])
                                     ([?\C-y] . [?\C-v])
                                     ;; search
                                     ([?\C-s] . [?\C-f])))
  (exwm-systemtray-enable))

(use-package dmenu
  :bind ("s-SPC" . 'dmenu))

(provide 'init-exwm)
