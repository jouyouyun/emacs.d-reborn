;;; module-company --- Company configurations.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up completion by company and lsp.

;;; Code:
(wen-require-packages '(company company-org-block company-tabnine))

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

;; Customize company backends.
(set (make-local-variable 'company-backends) '(
                                               (company-tabnine
                                                company-dabbrev
                                                company-keywords
                                                company-files)
                                               ))

;; Add `company-elisp' backend for elisp.
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (require 'company-elisp)
              (set (make-local-variable 'company-backends) '(
                                                             (company-tabnine
                                                              company-elisp
                                                              company-dabbrev
                                                              company-keywords
                                                              company-files)
                                                             ))))

;; Remove duplicate candidate.
(add-to-list 'company-transformers #'delete-dups)

;; org-block
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

(provide 'module-company)

;;; module-company.el ends here
