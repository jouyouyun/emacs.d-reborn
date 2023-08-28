;;; module-ai --- AI configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up dot by ai.

;;; Code:

;; org babel: #+begin_src chatgpt-shell #+end_src
(wen-require-packages '(shell-maker chatgpt-shell))

(use-package chatgpt-shell
  :ensure t
  :custom
  (setq chatgpt-shell-openai-key ""))

(provide 'module-ai)

;;; module-ai.el ends here
