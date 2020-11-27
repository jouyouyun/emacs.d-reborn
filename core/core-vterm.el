;;; core-vterm.el --- VTerm configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;;
;; This file sets up vterm.

;;; Code:


(wen-require-package 'vterm)
(wen-require-package 'multi-vterm)

(use-package vterm
  :ensure t)
(use-package multi-vterm
  :ensure t)

;; vterm configs
;; depends: libtool-bin
;; set zsh to enable directory tracking,
;;     see: https://github.com/akermu/emacs-libvterm/#directory-tracking-and-prompt-tracking
(global-set-key (kbd "C-c M-t") 'multi-vterm)
(global-set-key (kbd "C-c M-[") 'multi-vterm-prev)
(global-set-key (kbd "C-c M-]") 'multi-vterm-next)
(global-set-key (kbd "C-c M-p") 'multi-vterm-projectile)
(global-set-key (kbd "C-c C-j") 'vterm-copy-mode)
(global-set-key (kbd "C-c C-k") 'vterm-copy-mode-done)
;; <C-backspace> doesn't kill previous word
(define-key vterm-mode-map (kbd "<C-backspace>")
  (lambda () (interactive) (vterm-send-key (kbd "C-w"))))
;; counsel-yank-pop doesn't work
(defun vterm-counsel-yank-pop-action (orig-fun &rest args)
  (if (equal major-mode 'vterm-mode)
      (let ((inhibit-read-only t)
            (yank-undo-function (lambda (_start _end) (vterm-undo))))
        (cl-letf (((symbol-function 'insert-for-yank)
                   (lambda (str) (vterm-send-string str t))))
          (apply orig-fun args)))
    (apply orig-fun args)))

(advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action)

(provide 'core-vterm)

;;; core-vterm.el ends here
