;;; init.el --- Yuki's early Emacs init
;; Author: Muhammad Kaisar Arkhan (yuki_is_bored)

;;; Commentary:
;; Inspired by https://github.com/edvorg/emacs-configs

;;; Code:

;; GET OF MY LAWN REEEEEEE

;(package-initialize)

(require 'package)

;; Constants

(defconst my-init-dir "~/.emacs.d/init.d")
(defconst my-custom-file "~/.emacs.d/custom.el")

;; Turn off startup screen

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Please don't mess my shit

(setq auto-save-default nil)
(setq make-backup-files nil)
(setq custom-file my-custom-file)
(setq x-select-enable-clipboard nil)
(setq-default indent-tabs-mode nil)
(load custom-file 'noerr)

;; Hook real initialization script

(add-hook 'after-init-hook (lambda ()
                             (load "~/.emacs.d/init-real.el")))

;;; init.el ends here
