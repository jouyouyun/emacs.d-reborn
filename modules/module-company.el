;;; module-company --- Company configurations, using lsp as backend.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up completion by company and lsp.

;;; Code:
(wen-require-packages '(company company-lsp lsp-mode lsp-ui))

;; Enable 'company-fuzzy' if needed

(require 'company)
(setq company-idle-delay 0.5)
(setq company-show-numbers t)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)
;; invert the navigation direction if the the completion popup-isearch-match
;; is displayed on top (happens near the bottom of windows)
(setq company-tooltip-flip-when-above t)
(add-hook 'after-init-hook 'global-company-mode)
;; (global-company-mode 1)


(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :commands company-lsp)

(require 'lsp-ui)
(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
;; (define-key lsp-ui-mode-map (kbd "C-c C-l .") 'lsp-ui-peek-find-definitions)
;; (define-key lsp-ui-mode-map (kbd "C-c C-l ?") 'lsp-ui-peek-find-references)
;; (define-key lsp-ui-mode-map (kbd "C-c C-l r") 'lsp-rename)
;; (define-key lsp-ui-mode-map (kbd "C-c C-l x") 'lsp-restart-workspace)
;; (define-key lsp-ui-mode-map (kbd "C-c C-l w") 'lsp-ui-peek-find-workspace-symbol)
;; (define-key lsp-ui-mode-map (kbd "C-c C-l i") 'lsp-ui-peek-find-implementation)
;; (define-key lsp-ui-mode-map (kbd "C-c C-l d") 'lsp-describe-thing-at-point)
;; (define-key lsp-ui-mode-map (kbd "C-c C-l e") 'lsp-execute-code-action)
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-peek-enable t)
(setq lsp-ui-peek-always-show t)


(require 'company-lsp)
(require 'lsp-mode)

;; Configuration to fix LSP
;; we will got error "Wrong type argument: sequencep" from `eldoc-message' if `lsp-enable-eldoc' is non-nil
(setq lsp-enable-eldoc nil)
;; avoid popup warning buffer if lsp can't found root directory (such as edit simple *.py file)
(setq lsp-message-project-root-warning t)
;; we will got error "Error from the Language Server: FileNotFoundError" if `create-lockfiles' is non-nil
(setq create-lockfiles nil)

;;; GoLang
;; Dependencies:
;;     github.com/klauspost/asmfmt/cmd/asmfmt
;;     github.com/go-delve/delve/cmd/dlv
;;     github.com/kisielk/errcheck
;;     github.com/davidrjenni/reftools/cmd/fillstruct
;;     github.com/mdempsky/gocode
;;     github.com/stamblerre/gocode --- gocode for module
;;     github.com/rogpeppe/godef
;;     github.com/zmb3/gogetdoc
;;     golang.org/x/tools/cmd/goimports
;;     golang.org/x/lint/golint
;;     golang.org/x/tools/gopls@latest
;;     github.com/golangci/golangci-lint/cmd/golangci-lint
;;     github.com/fatih/gomodifytags
;;     golang.org/x/tools/cmd/gorename
;;     github.com/jstemmer/gotags
;;     golang.org/x/tools/cmd/guru
;;     github.com/josharian/impl
;;     honnef.co/go/tools/cmd/keyify
;;     github.com/fatih/motion
;;     github.com/koron/iferr
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
;; gopls
(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))


(provide 'module-company)

;;; module-company.el ends here