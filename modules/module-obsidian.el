;;; module-obsidian --- Obsidian configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up dot by obsidian.

;;; Code:

;; Depends: obisidian
(wen-require-packages '(obsidian))

(use-package obsidian
  :ensure t
  :demand t
  :config
  (obsidian-specify-path wen-module-obsidian-repo)
  (global-obsidian-mode t)
  :custom
  ;; This directory will be used for `obsidian-capture' if set.
  (obsidian-inbox-directory "Tasks")
  :bind (:map obsidian-mode-map
              ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
              ("C-c C-o" . obsidian-follow-link-at-point)
              ;; Jump to backlinks
              ("C-c C-b" . obsidian-backlink-jump)
              ;; If you prefer you can use `obsidian-insert-wikilink'
              ("C-c C-l" . obsidian-insert-link)))

(provide 'module-obsidian)

;;; module-obsidian.el ends here
