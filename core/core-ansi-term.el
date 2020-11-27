;;; core-ansi-term.el --- ANSI Term configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;;
;; This file sets up ansi-term.

;;; Code:


;; ansi-term configs
;; diretory tracking: add the following code to '.zshrc'
;;    if [ -n "$INSIDE_EMACS" ]; then
;;        chpwd() { print -P "\033AnSiTc %d" }
;;        print -P "\033AnSiTu %n"
;;        print -P "\033AnSiTc %d"
;;    fi
;; END
(global-set-key (kbd "C-c M-t") 'ansi-term)

;; kill buffer automatically after terminal exit
(defun wen-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(eval-after-load "term"
  `(progn
     ;; no limit buffer length
     (add-hook 'term-exec-hook 'wen-term-exec-hook)
     (setq show-trailing-whitespace nil)
     (setq term-bind-key-alist
           (list (cons "C-c C-c" 'term-interrupt-subjob)
                 ;; send 'ESC' to terminal
				 (cons "C-c M-e" 'term-send-esc)
                 (cons "C-p" 'previous-line)
                 (cons "C-n" 'next-line)
                 (cons "M-f" 'term-send-forward-word)
                 (cons "M-b" 'term-send-backward-word)
                 (cons "C-c C-j" 'term-line-mode)
                 (cons "C-c C-k" 'term-char-mode)
                 (cons "M-DEL" 'term-send-backward-kill-word)
                 (cons "M-d" 'term-send-forward-kill-word)
                 (cons "C-r" 'term-send-reverse-search-history)))
     ;; By default, C-y calls term's own yank, which is different from Emacs's yank.
     ;; So reset the keyboard shortcut.
     (define-key term-raw-map (kbd "C-y") 'term-paste)))


(provide 'core-ansi-term)

;;; core-ansi-term.el ends here
