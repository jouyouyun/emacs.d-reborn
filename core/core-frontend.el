;;; core-buffer.el --- Management buffer
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;;
;; This file sets up buffer management tools.

;;; Code:

;;; (when wen-frontend
;;;   (if (eq wen-frontend "ivy")
;;;       (require 'core-ivy))
;;;   (require 'core-helm))

(require 'core-ivy)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(defadvice set-buffer-major-mode (after set-major-mode activate compile)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (when (and mode (consp mode))
      (setq mode (car mode)))
    (with-current-buffer buffer (if mode (funcall mode)))))

(set-default 'imenu-auto-rescan t)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

(provide 'core-frontend)

;;; core-buffer.el ends here
