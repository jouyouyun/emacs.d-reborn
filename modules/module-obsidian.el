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
  :bind (
         ("C-c M-o" . obsidian-follow-link-at-point)
         ;; Jump to backlinks
         ("C-c M-b" . obsidian-backlink-jump)
         ;; If you prefer you can use `obsidian-insert-wikilink'
         ("C-c M-l" . obsidian-insert-link)
         ("C-c M-j" . obsidian-jump)
         ("C-c M-f" . obsidian-search)
         ("C-c M-u" . obsidian-update)))

(provide 'module-obsidian)

;;; module-obsidian.el ends here
