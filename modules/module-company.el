;;; module-company --- Company configurations.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up completion by company and lsp.

;;; Code:
(wen-require-packages '(company company-org-block company-box))

;; Enable 'company-fuzzy' if needed

(require 'company)
(setq company-idle-delay 0.3)
(setq company-show-numbers t)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)
;; invert the navigation direction if the the completion popup-isearch-match
;; is displayed on top (happens near the bottom of windows)
(setq company-tooltip-flip-when-above t)
(add-hook 'after-init-hook 'global-company-mode)
;; (global-company-mode 1)

;; show icons
(use-package company-box
  :ensure t
  :if window-system
  :hook (company-mode . company-box-mode))

;; Customize company backends.
(set (make-local-variable 'company-backends) '(
                                               (company-dabbrev
                                                company-keywords
                                                company-capf
                                                company-files)
                                               ))

;; Remove duplicate candidate.
(add-to-list 'company-transformers #'delete-dups)

;; complete filename replace hippie-expand
(global-set-key (kbd "M-/") 'company-files)

;; org-block
;; insert org block complete, prefix '<'
(use-package company-org-block
  :ensure t
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

(when wen-module-lsp-frame
  (message "Will set default lsp frame to %s" wen-module-lsp-frame)
  (if (equal wen-module-lsp-frame "lsp")
      (require 'module-lsp)
    (require 'module-lsp-bridge)))

;; company-tabnine cost high cpu
;; company-tabnine-install-binary
;;(wen-require-packages '(company-tabnine))
;;(use-package company-tabnine
;;  :ensure t
;;  :init (add-to-list 'company-backends #'company-tabnine))

(provide 'module-company)

;;; module-company.el ends here
