;;; init.el --- Emacs's configuration entry point
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>
;;

;;; Commentary:
;; This file will set up the modules load path and load these special modules.

;;; Code:
(defvar current-user
  (getenv "USER"))

(message "Emacs startup for %s..." current-user)

;; Version checker
(when (version< emacs-version "25.1")
  (error "Require Emacs 15.1 or newer, but you're running %s" emacs-version))

;; Always load newest byte code
;; Non-nil means load prefers the newest version of a file.
(setq load-prefer-newer t)

;; Sets up the modules load path and other configuration files.
(defvar config-dir (file-name-directory load-file-name)
  "Emacs configuration root dir.")
(defvar config-core-dir (expand-file-name "core" config-dir)
  "Emacs core modules dir.")
(defvar config-modules-dir (expand-file-name "modules" config-dir)
  "Emacs optional modules dir.")
(defvar config-personal-dir (expand-file-name "personal" config-dir)
  "Emacs personal modules dir.")
(defvar config-savefile-dir (expand-file-name "savefile" config-dir)
  "Emacs automatically generated files, such as: recently, history etc.")
(defvar config-modules-file (expand-file-name "loaded-modules.el" config-personal-dir)
  "This file contains a list of optional modules will be loaded.")

(unless (file-exists-p config-savefile-dir)
  (make-directory config-savefile-dir))

;; add configuration's directories to `load-path'
(add-to-list 'load-path config-core-dir)
(add-to-list 'load-path config-modules-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; proxy
;;(setq url-gateway-method 'socks)
;;(setq socks-server '("Server" "127.0.0.1" 1080 5))

(message "Loading core modules...")
(require 'core-packages)
(require 'core-custom) ;; if custom some variable in personal, please load at the next line
(require 'core-ui)
(require 'core-project) ;; must load before core-frontend
(require 'core-frontend)
(require 'core-window)
(require 'core-editor)
(require 'core-env-path)
(require 'core-terminal)
(require 'core-org)
(require 'core-tips)

(message "Loading optional modules...")
(if (file-exists-p config-modules-file)
    (progn
      (load config-modules-file))
  (message "Missing optional modules file %s" config-modules-file)
  (message "You can get started by copying the example file from sample/loaded-modules/el"))

;; load the personal modules, filter the file 'config-modules-file'
(when (file-exists-p config-personal-dir)
  (message "Loading personal modules in %s..." config-personal-dir)
  (mapc 'load (delete
               config-modules-file
               (directory-files config-personal-dir 't "^[^#\.].*\\.el$"))))

(message "Emacs is ready for %s..." current-user)

;; Patch security vulnerability in Emacs versions older than 25.3
(when (version< emacs-version "25.3")
  (with-eval-after-load "enriched"
    (defun enriched-decode-display-prop (start end &optional param)
      (list start end))))

(wen-eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'wen-tip-of-the-day))

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(ox-mediawiki ox-gfm htmlize go-mode zop-to-char zenburn-theme whole-line-or-region which-key volatile-highlights use-package undo-tree super-save smartrep smartparens seethru popup-kill-ring operate-on-number multiple-cursors multi-term lsp-ivy guru-mode gitignore-mode gitconfig-mode git-timemachine gist expand-region exec-path-from-shell editorconfig easy-kill diminish diff-hl crux counsel-tramp counsel-projectile beacon async anzu all-the-icons-ivy ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
