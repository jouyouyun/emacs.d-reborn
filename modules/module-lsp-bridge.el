;;; module-lsp-bridge --- LSP Bridge configurations.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up lsp bridge.

;;; Code:

;; depends: python-epc
(wen-require-packages '(orderless corfu))

(add-to-list 'load-path (expand-file-name "lsp-bridge" config-personal-package-dir))

(require 'lsp-bridge)             ;; load lsp-bridge
(global-corfu-mode)               ;; use corfu as completion ui

(require 'lsp-bridge-orderless)   ;; make lsp-bridge support fuzzy match, optional
(require 'lsp-bridge-icon)        ;; show icon for completion items, optional

;; Enable auto completion in elisp mode.
(dolist (hook (list
               'emacs-lisp-mode-hook
               ))
  (add-hook hook (lambda ()
                   (setq-local corfu-auto t)
                   )))

;; Enable lsp-bridge.
(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'python-mode-hook
               'ruby-mode-hook
               'rust-mode-hook
               'elixir-mode-hook
               'go-mode-hook
               'haskell-mode-hook
               'haskell-literate-mode-hook
               'dart-mode-hook
               'scala-mode-hook
               'typescript-mode-hook
               'typescript-tsx-mode-hook
               'js2-mode-hook
               'js-mode-hook
               'rjsx-mode-hook
               'tuareg-mode-hook
               'latex-mode-hook
               'Tex-latex-mode-hook
               'texmode-hook
               'context-mode-hook
               'texinfo-mode-hook
               'bibtex-mode-hook
               'clojure-mode-hook
               'clojurec-mode-hook
               'clojurescript-mode-hook
               'clojurex-mode-hook
               'sh-mode-hook
               'web-mode-hook
               ))
  (add-hook hook (lambda ()
                   (setq-local corfu-auto nil)  ;; let lsp-bridge control when popup completion frame
                   (lsp-bridge-mode 1)
                   )))

(provide 'module-lsp-bridge)

;;; module-lsp-bridge.el ends here
