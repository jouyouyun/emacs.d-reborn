;;; module-copilot --- Copilot configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up dot by copilot.

;;; Code:

(wen-require-packages '(editorconfig jsonrpc))

;; Install
;;   1. emacs > 27, and the dependencies(editorconfig jsonrpc) has installed
;;   2. nodejs > 18
;;   3. copilot-emacs not in melpa, download
;;     3.1 mkdir ~/.emacs.d/elpa/copilot-manual
;;     3.2 wget https://github.com/copilot-emacs/copilot.el/raw/main/Eask -O ~/.emacs.d/elpa/copilot.el/Eask
;;     3.3 wget https://github.com/copilot-emacs/copilot.el/raw/main/LICENSE -O ~/.emacs.d/elpa/copilot.el/LICENSE
;;     3.4 wget https://github.com/copilot-emacs/copilot.el/raw/main/README.md -O ~/.emacs.d/elpa/copilot.el/README.md
;;     3.5 wget https://github.com/copilot-emacs/copilot.el/raw/main/copilot-balancer.el -O ~/.emacs.d/elpa/copilot.el/copilot-balancer.el
;;     3.6 wget https://github.com/copilot-emacs/copilot.el/raw/main/copilot.el -O ~/.emacs.d/elpa/copilot.el/copilot.el
;;   4. install server by `M-x copilot-install-server`
;;   5. login by `M-x copilot-login`, check status by `M-x copilot-diagnose`

(add-to-list 'load-path (expand-file-name "elpa/copilot-manual" config-dir))
(require 'copilot)
;; Use copilot-mode to automatically provide completions
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(provide 'module-copilot)

;;; module-copilot.el ends here
