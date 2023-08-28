;;; core-env-path.el --- Environment path configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;;
;; This file sets up environment variables.

;;; Code:
(wen-require-packages '(exec-path-from-shell))

(require 'exec-path-from-shell)
(when (daemonp)
  (exec-path-from-shell-initialize))

(provide 'core-env-path)

;;; core-env-path.el ends here
