;;; module-chatgpt --- ChatGPT configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up dot by chatgpt.

;;; Code:

(wen-require-packages '(chatgpt-shell))

(use-package chatgpt-shell
  :ensure t
  ;; :custom
  ;; ((setq chatgpt-shell-openai-key wen-module-openai-key))
  )
(setq chatgpt-shell-openai-key wen-module-openai-key)

(provide 'module-chatgpt)

;;; module-chatgpt.el ends here
