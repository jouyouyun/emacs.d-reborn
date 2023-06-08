;;; module-org-roam --- org-roam configurations.
;;
;; 为了支持 Logseq，node 全部放在 pages 下，日记则在 journals 下
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up org-roam.

;;; Code:

(wen-require-packages '(org-roam org-roam-ui org-download org-ref))

(defvar config-roam-db-dir (expand-file-name "roam-db" config-dir)
  "Org Roam db directory.")
(unless (file-exists-p config-roam-db-dir)
  (make-directory config-roam-db-dir))

;; fixed ox-hugo export md error: unable to resolve link
;; (require 'find-lisp)

(defun wen-roam-set-directory (dir)
  (setq wen-db-name (replace-regexp-in-string "/" "" dir))
  (setq wen-db-name (concat wen-db-name ".db"))
  (message "Switch roam directory to %s" dir)
  (setq org-roam-directory dir)
  ;; (setq org-roam-dailies-directory "journals/")
  (message "Set roam db to %s" (expand-file-name wen-db-name config-roam-db-dir))
  (setq org-roam-db-location (expand-file-name wen-db-name config-roam-db-dir))
  ;; fixed ox-hugo export md error: unable to resolve link
  ;; (setq org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$"))
  ;; 定义 agenda 文件的位置
  (setq org-agenda-files (list (expand-file-name "journals" dir)))
)

(defun wen-roam-switch ()
  (interactive)
  (wen-roam-set-directory (read-directory-name
                           "Org Roam directory:" default-directory))
  )

(use-package org-roam
  :ensure t
  ;; :custom
  ;; If the dir is symbolic, must use file-truename
  ;; (org-roam-directory (file-truename "~/Documents/Notes/Roam/"))
  ;; mkdir -p ~/Documents/Notes/Roam/daily
  ;; (org-roam-directory "~/Documents/Notes/Roam/")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:100}" 'face 'org-tag)))
  ;; default roam dir
  (org-roam-db-autosync-mode)
  (setq org-roam-db-gc-threshold 50000000)
  ;; If using org-roam-protocol
  ;; (require 'org-roam-protocol)
  (setq org-roam-capture-templates '(
                                     ("p" "Post" plain "%?"
                                      :target (file+head "pages/${title}.org"
                                                         "#+OPTIONS: author:jouyouyun timestamp:nil ^:nil <:nil p:t prop:t tags:t tasks:t todo:t\n#+TITLE: ${title}\n#+DATE: %<%Y-%m-%dT%H:%M:%S+08:00>\n#+FILETAGS: tag\n#+HUGO_BASE_DIR: /Data/Projects/Private/blog-with-hugo\n#+HUGO_SECTION: post\n#+HUGO_CATEGORIES: cate\n#+HUGO_TAGS: tag\n#+HUGO_AUTO_SET_LASTMOD: nil\n#+HUGO_DRAFT: false\n")
                                      :unnarrowed t)
                                     ("t" "Task" entry
                                      "* TODO [#B] ${title}%? :TAGS:\n:PROPERTIES:\n:END:\n"
                                      :target (file+head "journals/task.org"
                                                         "#+OPTIONS: author:jouyouyun timestamp:nil ^:nil <:nil p:t prop:t tags:t tasks:t todo:t\n#+TITLE: Task Management\n#+FILETAGS: task\n")
                                      :unnarrowed t)
                                     ("d" "Daily" entry
                                      "*** %<%d>\n%?\n"
                                      :target (file+head "journals/daily.org"
                                                         "#+OPTIONS: author:jouyouyun timestamp:nil ^:nil <:nil p:t prop:t tags:t tasks:t todo:t\n#+TITLE: Daily\n#+FILETAGS: daily\n")
                                      :unnarrowed t)
                                     ))
  (require 'org-roam-export)
  ;; (setq org-roam-dailies-directory "journals/")
  ;; (setq org-roam-dailies-capture-templates
  ;;       '("d" "Daily" entry
  ;;         "* %?"
  ;;         :target (file+head "%<%Y-%m-%d>.org"
  ;;                            "#+OPTIONS: author:jouyouyun timestamp:nil ^:nil <:nil p:t prop:t tags:t tasks:t todo:t\n#+TITLE: %<%Y-%m-%d>\n#+FILETAGS: journal\n")
  ;;         :unnarrowed t))
  )

;; set default roam dir
(wen-roam-set-directory (expand-file-name  "daily-in-org-roam" wen-daily-sync-repo))
;; 忽略 logseq 配置目录
(setq org-roam-file-exclude-regexp ".*_archived.org"
      ;; (concat "^" (expand-file-name org-roam-directory) "/archived/")
)

(global-set-key (kbd "C-c n p") 'wen-roam-switch)

(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme nil
        org-roam-ui-follow nil
        org-roam-ui-update-on-save nil
        org-roam-ui-open-on-start nil))

(provide 'module-org-roam)

;;; module-org-roam.el ends here
