;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! webfeeder)
(package! direnv)
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
(unpin! haskell-mode)
(package! modus-themes)
(package! wakatime-mode)
