;;; module-ellama --- Ellama configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up dot by ellama.

;;; Code:

(wen-require-packages '(llm ellama))

(use-package ellama
  :init
  ;; setup key bindings
  (setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (setopt ellama-language "Chinese")
  ;; could be llm-openai for example
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           :scheme wen-module-ai-scheme
           :host wen-module-ai-host
           :port wen-module-ai-port
           :chat-model wen-module-ai-chat-model
           :embedding-model wen-module-ai-embedding-model))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
          (make-llm-ollama
           :scheme wen-module-ai-scheme
           :host wen-module-ai-host
           :port wen-module-ai-port
           :chat-model wen-module-ai-chat-model
           :embedding-model wen-module-ai-embedding-model))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translation llm provider
  (setopt ellama-translation-provider (make-llm-ollama
                                       :chat-model wen-module-ai-chat-model
                                       :embedding-model wen-module-ai-embedding-model)))

(provide 'module-ellama)

;;; module-ellama.el ends here
