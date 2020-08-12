;;; core-helm.el --- Helm configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;;
;; This file sets up helm.

;;; Code:
(wen-require-packages '(helm helm-projectile helm-ag helm-lsp))

(require 'helm-config)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(when (executable-find "ag")
  (global-set-key (kbd "s-?") 'helm-ag))

;; See https://github.com/bbatsov/prelude/pull/670 for a detailed
;; discussion of these options.
(setq helm-split-window-in-side-p           t
      helm-buffers-fuzzy-matching           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-ff-file-name-history-use-recentf t)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;;(define-key helm-command-map (kbd "o")     'helm-occur)
;;(define-key helm-command-map (kbd "g")     'helm-do-grep)
;;(define-key helm-command-map (kbd "C-c w") 'helm-wikipedia-suggest)
;;(define-key helm-command-map (kbd "SPC")   'helm-all-mark-rings)

(require 'helm-projectile)
(helm-projectile-on)

(provide 'core-helm)

;;; core-helm.el ends here
