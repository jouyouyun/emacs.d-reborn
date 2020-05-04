;;; core-project.el --- Project configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;;
;; This file enable projectile, migrate to helm or ivy.

;;; Code:
(require 'projectile)

;; Details see core-ivy.el or core-helm.el
(setq projectile-cache-file (expand-file-name  "projectile.cache" config-savefile-dir))
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(provide 'core-project)

;;; core-project.el ends here
