;;; module-company --- Company configurations, using lsp as backend.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up completion by company and lsp.

;;; Code:
(wen-require-packages '(company lsp-mode lsp-ui lsp-ivy go-snippets treemacs lsp-treemacs company-org-block company-tabnine))
;; (wen-require-packages '(company lsp-mode lsp-ui ccls lsp-ivy go-snippets treemacs lsp-treemacs))

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

;; Customize company backends.
(setq company-backends
      '(
        (company-tabnine company-dabbrev company-keywords company-files company-capf)
        ))

;; Add `company-elisp' backend for elisp.
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (require 'company-elisp)
              (push 'company-elisp company-backends)))

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

(use-package lsp-mode
  :custom
  (lsp-ui-sideline-show-diagnostics nil)
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

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


(require 'lsp-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; Configuration to fix LSP
;; we will got error "Wrong type argument: sequencep" from `eldoc-message' if `lsp-enable-eldoc' is non-nil
(setq lsp-enable-eldoc nil)
;; avoid popup warning buffer if lsp can't found root directory (such as edit simple *.py file)
(setq lsp-message-project-root-warning t)
;; we will got error "Error from the Language Server: FileNotFoundError" if `create-lockfiles' is non-nil
(setq create-lockfiles nil)
;; enable treemacs sync
(lsp-treemacs-sync-mode 1)

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


;; ccls
;;;(use-package ccls
;;;  :ensure t
;;;  :config
;;;  (setq lsp-prefer-flymake nil)
;;;  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
;;;  :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;;         (lambda () (require 'ccls) (lsp))))

;; LaTex
(defun wen-lsp-latex()
  ;; dependencies: texlab
  (wen-require-package 'lsp-latex)
  (use-package lsp-latex
    :ensure t
    :config
    ;; "texlab" must be located at a directory contained in `exec-path'.
    ;; If you want to put "texlab" somewhere else,
    ;; you can specify the path to "texlab" as follows:
    ;; (setq lsp-latex-texlab-executable "/usr/local/bin/texlab"))

    (add-hook 'latex-mode-hook
              (lambda ()
                ;; Use 'luatex' as default tex engine
                (setq TeX-engine 'luatex
                      TeX-show-compilation t)
                (add-to-list 'tex-compile-commands '("lualatex -interaction=nonstopmode --shell-escape --synctex=1 %f" t "%r.pdf"))
                (setq lsp-latex-build-executable "lualatex") ;; why not work, still use 'latexmk'? Now add the file '~/.latexmkrc'
                (setq lsp-latex-build-args '("-lualatex" "-interaction=nonstopmode" "--shell-escape" "-synctex=1" "%f")))))
  (setq lsp-tex-server 'texlab))

(when wen-tex-server
  (message "Will set tex server to %s" wen-tex-server)
  (if (equal wen-tex-server "digestif")
      ;; Depends: luarocks digestif
      ;; Install: sudo apt install liblua5.3-0 liblua5.3-dev
      ;; texlab cause cpu 100%, using digestif as default
      (setq lsp-tex-server 'digestif)
    (wen-lsp-latex)))

(with-eval-after-load "tex-mode"
  (add-hook 'tex-mode-hook 'lsp)
  (add-hook 'latex-mode-hook 'lsp))
;; For YaTeX
(with-eval-after-load "yatex"
  (add-hook 'yatex-mode-hook 'lsp))
;; For bibtex
(with-eval-after-load "bibtex"
  (add-hook 'bibtex-mode-hook 'lsp))

(defun wen-latex-build-clean()
  (interactive)
  (shell-command "cp -f *.pdf /tmp/ || /bin/true")
  (shell-command "rm -f *.fdb_latexmk *.fls || /bin/true") ;; for 'lsp-latex-build'
  (shell-command "rm -f *.latex || /bin/true")
  (shell-command "rm -f *.pdf || /bin/true")
  (shell-command "rm -f *.txt || /bin/true") ;; plantuml
  (shell-command "rm -f *.aux *.log *.out *.toc *.gz"))

;; python
(wen-require-packages '(lsp-python-ms))
(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))  ; or lsp-deferred


(provide 'module-company)

;;; module-company.el ends here
