;;; module-d2 --- D2 configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up dot by d2.

;;; Code:

;; Depends: d2
;;  wget https://d2lang.com/install.sh
;;  chmod +x install.sh
;;  sudo ./install.sh --prefix=/usr/local
;; (wen-require-packages '(d2-mode ob-d2))
(wen-require-packages '(d2-mode))

(when (executable-find "d2")
  ;; d2-mode
  (message "enable d2-mode")
  (require 'd2-mode)
  (add-to-list 'auto-mode-alist '("\\.d2\\'" . d2-mode))
  ;; Integration with org-mode
  (add-to-list 'org-src-lang-modes '("d2" . d2))
  ;; ob-d2 enables Org-Babel support for evaluating d2 code
  ;; not found in elpa mirror
  ;;(use-package org
  ;;  :after ob-d2
  ;;  :config
  ;;  (org-babel-do-load-languages
  ;;   'org-babel-load-languages
  ;;   '((d2 . t))))
  )

(provide 'module-d2)

;;; module-d2.el ends here
