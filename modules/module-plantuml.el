;;; module-plantuml --- PlantUML configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up dot by plantuml.

;;; Code:

;; Depends: graphviz, default-jdk
(wen-require-packages '(plantuml-mode flycheck-plantuml))

;; Enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
;; Integration with org-mode
(add-to-list
 'org-src-lang-modes '("plantuml" . plantuml))

(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

;; Sample jar configuration
(setq plantuml-jar-path (expand-file-name "plantuml.jar" config-misc-dir))
(setq org-plantuml-jar-path (expand-file-name "plantuml.jar" config-misc-dir))
(setq plantuml-default-exec-mode 'jar)

;; server mode
;; (setq plantuml-default-exec-mode 'server)
;; (setq plantuml-server-url "https://www.plantuml.com/plantuml")

(with-eval-after-load 'flycheck
  (require 'flycheck-plantuml)
  (flycheck-plantuml-setup))


(provide 'module-plantuml)

;;; module-plantuml.el ends here
