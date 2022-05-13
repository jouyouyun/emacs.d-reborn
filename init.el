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
(defvar config-personal-package-dir (expand-file-name "packages" config-personal-dir)
  "Emacs personal packages dir.")
(defvar config-savefile-dir (expand-file-name "savefile" config-dir)
  "Emacs automatically generated files, such as: recently, history etc.")
(defvar config-modules-file (expand-file-name "loaded-modules.el" config-personal-dir)
  "This file contains a list of optional modules will be loaded.")
(defvar config-misc-dir (expand-file-name "misc" config-dir)
  "Emacs misc dir.")

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" config-personal-dir))

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
(require 'core-editor)
(require 'core-env-path)
(require 'core-org)
(require 'core-chinese)
(require 'core-autoinsert)
(require 'core-treemacs)
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

;; Must be loaded after 'custom.el'
(require 'core-frontend)
(require 'core-terminal)
(require 'core-window)

(message "Emacs is ready for %s..." current-user)

;; Patch security vulnerability in Emacs versions older than 25.3
(when (version< emacs-version "25.3")
  (with-eval-after-load "enriched"
    (defun enriched-decode-display-prop (start end &optional param)
      (list start end))))

;; Disable 'C-z'
;; 'C-s' resume from 'C-z'
(global-unset-key (kbd "C-z"))

;; enable 'pdf-tools'
;;(pdf-tools-install)

(wen-eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'wen-tip-of-the-day))
