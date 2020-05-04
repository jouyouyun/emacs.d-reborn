;;; core-window.el --- Widnow configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;;
;; This file sets up window management shortcuts.

;;; Code:
(require 'ace-window)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x o") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one
(global-set-key [remap other-window] 'ace-window)

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(require 'super-save)
;; add integration with ace-window
(add-to-list 'super-save-triggers 'ace-window)
(super-save-mode +1)

(provide 'core-window)

;;; core-window.el ends here
