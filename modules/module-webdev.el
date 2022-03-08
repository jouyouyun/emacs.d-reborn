;;; module-webdev --- webdev configurations, using lsp as backend.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up web completion by lsp.

;;; Code:
(wen-require-packages '(emmet-mode web-beautify typescript-mode tide))

;; web tools
(use-package emmet-mode)
;; (use-package web-mode
;;   :config
;;   (progn
;;     (defun @-web-mode-hook ()
;;       "Hooks for Web mode."
;;       (setq web-mode-markup-indent-offset 4)
;;       (setq web-mode-code-indent-offset 4)
;;       (setq web-mode-css-indent-offset 4))

;;     (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
;;     (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;;     (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
;;     (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

;;     (add-hook 'web-mode-hook  '@-web-mode-hook)    
;;     (setq tab-width 4)

;;     (add-hook 'web-mode-hook  'emmet-mode)))
(use-package web-beautify)

;; typescirpt tide
(use-package typescript-mode)
(use-package tide)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(provide 'module-webdev)

;;; module-webdev.el ends here
