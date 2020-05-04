;;; module-gtags --- GTags configurations.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up gtags.

;;; Code:

(wen-require-packages '(ggtags))

(defun wen-gtags-produce-tags-if-needed (dir)
   (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
      (let ((default-directory dir))
        (shell-command "gtags")
        (message "tagfile created by GNU Global"))
    ;;  tagfile already exists; update it
    (shell-command "global -u")
    (message "tagfile updated by GNU Global")))

;; @see http://emacs-fu.blogspot.com.au/2008/01/navigating-through-source-code-using.html
(defun wen-gtags-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (wen-gtags-produce-tags-if-needed (read-directory-name
                            "gtags: top of source tree:" default-directory)))

(defun wen-gtags-add-gtagslibpath (libdir &optional del)
  "add external library directory to environment variable GTAGSLIBPATH.\ngtags will can that directory if needed.\nC-u M-x add-gtagslibpath will remove the directory from GTAGSLIBPATH."
  (interactive "DDirectory containing GTAGS:\nP")
  (let (sl)
  (if (not (file-exists-p (concat (file-name-as-directory libdir) "GTAGS")))
      ;; create tags
      (let ((default-directory libdir))
        (shell-command "gtags")
        (message "tagfile created by GNU Global")))

  (setq libdir (directory-file-name libdir)) ;remove final slash
  (setq sl (split-string (if (getenv "GTAGSLIBPATH") (getenv "GTAGSLIBPATH") "")  ":" t))
  (if del (setq sl (delete libdir sl)) (add-to-list 'sl libdir t))
  (setenv "GTAGSLIBPATH" (mapconcat 'identity sl ":"))
  ))

(defun wen-gtags-print-gtagslibpath ()
  "print the GTAGSLIBPATH (for debug purpose)"
  (interactive)
  (message "GTAGSLIBPATH=%s" (getenv "GTAGSLIBPATH")))

(require 'ggtags)
;; company can use ggtags as completion source via company-capf which is enabled by default.
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1)
              (push 'company-capf company-backends))))


(provide 'module-gtags)

;;; module-gtags.el ends here
