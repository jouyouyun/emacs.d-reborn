;;; module-c --- Calendar configurations.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up calendar.

;;; Code:
(wen-require-packages '(org-caldav))

(require 'org-caldav)

(defun wen-caldav-google ()
  (setq org-caldav-url "https://apidata.googleusercontent.com/caldav/v2")
  (setq org-caldav-calendar-id (concat wen-module-caldav-username "/events"))
  (setq org-caldav-oauth2-client-id wen-module-caldav-client-id)
  (setq org-caldav-oauth2-client-secret wen-module-caldav-client-secret)
  (setq plstore-cache-passphrase-for-symmetric-encryption t)
  )

;; from http://smallzhan.github.io/emacs/2021/07/25/p-caldav-emacs.html
(defun wen-caldav-dingtalk ()
  (setq org-caldav-url (concat "https://calendar.dingtalk.com/dav/" wen-module-caldav-username))
  (setq org-caldav-calendar-id "primary")
  (setq org-caldav-uuid-extension "")
  )

;; nextcloud calendar
(defun wen-caldav-nextcloud ()
  (setq org-caldav-url wen-module-caldav-url)
  (setq org-caldav-calendar-id (concat wen-module-caldav-username "/-"))
  )

(defun browse-url-firefox (url &optional new-window)
  "Ask the Firefox WWW browser to load URL.
  Default to the URL around or before point.  The strings in
  variable `browse-url-firefox-arguments' are also passed to
  Firefox.
  
  When called interactively, if variable
  `browse-url-new-window-flag' is non-nil, load the document in a
  new Firefox window, otherwise use a random existing one.  A
  non-nil interactive prefix argument reverses the effect of
  `browse-url-new-window-flag'.
  
  If `browse-url-firefox-new-window-is-tab' is non-nil, then
  whenever a document would otherwise be loaded in a new window, it
  is loaded in a new tab in an existing window instead.
  
  When called non-interactively, optional second argument
  NEW-WINDOW is used instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment))
         (window-args (if (browse-url-maybe-new-window new-window)
                          (if browse-url-firefox-new-window-is-tab
                              '("-new-tab")
                            '("-new-window"))))
         (ff-args (append browse-url-firefox-arguments window-args (list url)))
         (process-name (concat "firefox " url))
         (process (apply 'start-process process-name nil
                         browse-url-firefox-program ff-args) )) ))
;; for google oauth2
(setq browse-url-browser-function 'browse-url-firefox)

(use-package org-caldav
  :after org
  :init
  (when wen-module-caldav-provider
    (message "Will set caldav provider to %s" wen-module-caldav-provider)
    (if (equal wen-module-caldav-provider "google")
        (wen-caldav-google))
    (if (equal wen-module-caldav-provider "dingtalk")
        (wen-caldav-dingtalk))
    (if (equal wen-module-caldav-provider "nextcloud")
        (wen-caldav-nextcloud))
    )
  (setq org-caldav-sync-direction 'twoway)
  (setq org-icalendar-timezone "Asia/Shanghai")
  (setq org-caldav-inbox (expand-file-name "TODO/calendar.org" wen-knowledge-repo))
  (setq org-caldav-files `(,org-caldav-inbox))
  (setq org-caldav-save-directory config-personal-dir)
  (add-to-list 'org-agenda-files org-caldav-inbox)
  :config
  (require 'org-caldav))

(provide 'module-caldav)

;;; module-c.el ends here
