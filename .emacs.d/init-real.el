;;; init-real.el --- It's still better than systemd(tm)
;; Author: Muhammad Kaisar Arkhan (yuki_is_bored)

;;; Commentary:

;;; Code:

;; Start Emacs Server

(require 'server)
(unless (server-running-p)
  (server-start))

;; Recompile configuration files

(add-hook 'kill-emacs-hook (lambda()
                             (byte-recompile-directory my-init-dir 0 t)))

;; ELPA

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/")))

(eval-when-compile (package-initialize))

(defun require-package (package)
  "refresh package archives, check package presence and install if it's not installed"
  (if (null (require package nil t))
      (progn (let* ((ARCHIVES (if (null package-archive-contents)
                                  (progn (package-refresh-contents)
                                         package-archive-contents)
                                package-archive-contents))
                    (AVAIL (assoc package ARCHIVES)))
               (if AVAIL
                   (package-install package)))
             (require package))))

;; Install use-package

(require-package 'use-package)
(require 'use-package)

;; Install req-package

(require-package 'req-package)
(require 'req-package)

;; Load the init scripts

(req-package load-dir
  :force true
  :init
  (setq force-load-messages nil)
  (setq load-dir-debug nil)
  (setq load-dir-recursive t)
  :config
  (load-dir-one my-init-dir)
  (req-package-finish))

;;; init-real.el ends here
