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
(setq explicit-shell-file-name "/bin/zsh")
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

(add-hook 'term-exec-hook 'wen-term-exec-hook)

;; By default, C-y calls term's own yank, which is different from Emacs's yank.
;; So reset the keyboard shortcut.
(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

(provide 'core-ansi-term)

;;; core-ansi-term.el ends here
