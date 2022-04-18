;;; module-org-roam --- org-roam configurations.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up org-roam.

;;; Code:

(wen-require-packages '(org-roam org-roam-ui org-roam-timestamps org-download org-ref))

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
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-capture-templates '(
                                     ("d" "default" plain "%?"
                                      :target (file+head "${slug}.org"
                                                         "#+title: ${title}\n")
                                      :unnarrowed t)))
  (org-roam-db-autosync-mode)
  (setq org-roam-db-gc-threshold 50000000)
  ;; If using org-roam-protocol
  ;; (require 'org-roam-protocol)
  (require 'org-roam-export)
  ;; (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates '(
          ;; 设置 capture 模板
          ("t" "task" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("b" "book reading" plain "%?"
           :target (file+head "book/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          )))

(defvar config-roam-db-dir (expand-file-name "roam-db" config-dir)
  "Org Roam db directory.")
(unless (file-exists-p config-roam-db-dir)
  (make-directory config-roam-db-dir))

;; fixed ox-hugo export md error: unable to resolve link
;; (require 'find-lisp)

(defun wen-roam-set-directory (dir)
  (setq wen-db-name (replace-regexp-in-string "/" "_" dir))
  (setq wen-db-name (concat wen-db-name ".db"))
  (message "Switch roam directory to %s" dir)
  (setq org-roam-directory dir)
  (message "Set roam db to %s" (expand-file-name wen-db-name config-roam-db-dir))
  (setq org-roam-db-location (expand-file-name wen-db-name config-roam-db-dir))
  ;; fixed ox-hugo export md error: unable to resolve link
  ;; (setq org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$"))
  )

;; default roam dir
(wen-roam-set-directory (expand-file-name  "PersonalKnowledgeGraph" "~/Documents/"))

(defun wen-roam-switch ()
  (interactive)
  (wen-roam-set-directory (read-directory-name
                           "Org Roam directory:" default-directory))
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

(use-package org-roam-timestamps
  :after org-roam
  :config
  (org-roam-timestamps-mode))


;; Roam publish
(defun wen-roam-sitemap (title list)
  (message "Generate siemap %s - %s:" title list)
  (concat "#+OPTIONS: ^:nil author:nil html-postamble:nil\n"
          "#+TITLE: " title "\n\n"
          (org-list-to-org list) "\nfile:sitemap.svg"))

(setq wen-publish-time 0) ;; see the next section for context
(defun wen-roam-publication-wrapper (plist filename pubdir)
  (org-roam-graph)
  (org-html-publish-to-html plist filename pubdir)
  (setq wen-publish-time (cadr (current-time))))

(defun wen-org-roam-custom-link-builder (node)
  (let ((file (org-roam-node-file node)))
    (concat (file-name-base file) ".html")))

(defun wen-roam-export (dir)
  (unless (file-exists-p dir)
    (make-directory dir))
  (setq pubdir (replace-regexp-in-string "/" "-" dir))
  (setq pubdir (concat "pages" pubdir))
  (setq pubdir (expand-file-name pubdir "/tmp"))
  (unless (file-exists-p pubdir)
    (make-directory pubdir))
  (message "Will export '%s' to '%s'" dir pubdir)
  (setq org-publish-project-alist
        '(("roam"
           :base-directory "~/Downloads/Tmp/Note"
           :auto-sitemap t
           :sitemap-function wen-roam-sitemap
           :sitemap-title "Roam notes"
           :publishing-function wen-roam-publication-wrapper
           :publishing-directory "/tmp/shm/tmp"
           :section-number nil
           :table-of-contents nil
           )))
  ;; override the default link creation function
  (setq org-roam-graph-link-builder 'wen-org-roam-custom-link-builder)
  ;; copying the generated file to the export dir
  ;; depends: graphviz
  (add-hook 'org-roam-graph-generation-hook
            (lambda (dot svg) (if (< (- (cadr (current-time)) wen-publish-time) 5)
                                  (progn (copy-file svg (expand-file-name "sitemap.svg" "/tmp/shm/tmp/") 't)
                                         (kill-buffer (file-name-nondirectory svg))
                                         (setq wen-publish-time 0)))))
  )

(defun wen-roam-publish-switch ()
  (interactive)
  (wen-roam-export (read-directory-name
                    "Org Roam Export Dir:" default-directory)))

(provide 'module-org-roam)

;;; module-org-roam.el ends here
