;;; module-graphviz --- Graphviz configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up dot by graphviz.

;;; Code:
(wen-require-packages '(graphviz-dot-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t))) ; this line activates dot

;; Dependencies: graphviz
;; Links: https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-dot.html
;;        https://coldnew.github.io/787b7d73/
;;        https://brantou.github.io/2018/04/23/ob-dot-intro/
(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package company-graphviz-dot
  )

(provide 'module-graphviz)

;;; module-graphviz.el ends here
