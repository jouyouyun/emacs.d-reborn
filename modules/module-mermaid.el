;;; module-mermaid --- Mermaid configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up dot by mermaid.

;;; Code:

;; Depends: mermaid-cli
;;    sudo npm install -g @mermaid-js/mermaid-cli
;;  or using docker
;;    docker  pull minlag/mermaid-cli:latest
;;    (setq mermaid-mmdc-location "docker")
;;    (setq mermaid-flags "run -u 1000 -v /tmp:/tmp minlag/mermaid-cli:latest")
(wen-require-packages '(mermaid-mode ob-mermaid))

(when (executable-find "mmdc")
  ;; mermaid-mode
  (message "enable mermaid-mode")
  (require 'mermaid-mode)
  (add-to-list 'auto-mode-alist '("\\.mmdc\\'" . mermaid-mode))
  (add-to-list 'auto-mode-alist '("\\.mermaid\\'" . mermaid-mode))
  ;; Integration with org-mode
  (add-to-list 'org-src-lang-modes '("mermaid" . mermaid))
  ;; ob-mermaid enables Org-Babel support for evaluating mermaid code
  (use-package org
    :after ob-mermaid
    :config
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((mermaid . t))))
  )

(provide 'module-mermaid)

;;; module-mermaid.el ends here
