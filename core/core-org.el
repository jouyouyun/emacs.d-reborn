;;; core-org.el --- org-mode configuration.
;;
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>
;; URL: https://github.com/jouyouyun/emacs.d-reborn

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for org-mode.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-switchb)
(setq org-log-done t)

(defun wen-org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.css"))
           (homestyle (or (null dir) (null (file-exists-p path))))
           (final (if homestyle "~/.emacs.d/templates/org-html.css" path))) ;; <- set your own style file path
      (setq org-html-head-include-default-style nil)
      (setq org-html-checkbox-type 'html) ;; <- enable checkout in html
      (setq org-html-validation-link nil) ;; <- remove validate in bottom
      (setq org-html-head (concat
                           "<style type=\"text/css\">\n"
                           "<!--/*--><![CDATA[/*><!--*/\n"
                           (with-temp-buffer
                             (insert-file-contents final)
                             (buffer-string))
                           "/*]]>*/-->\n"
                           "</style>\n")))))

(add-hook 'org-export-before-processing-hook 'wen-org-inline-css-hook)

;;; markdown
(wen-require-package 'ox-gfm)
(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; markdown review
;; flymd
;; see: http://devlz.com/2016/08/07/emacs-Markdown%E5%AE%9E%E6%97%B6%E9%A2%84%E8%A7%88/
(wen-require-package 'flymd)
(defun my-flymd-browser-function (url)
  (let ((browser-url-browser-function 'browse-url-firefox))
    (browse-url url)))
(setq flymd-browser-function 'my-flymd-browser-function)
(setq flymd-output-directory "/tmp")

(provide 'core-org)

;;; core-org.el ends here
