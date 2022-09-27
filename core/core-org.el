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


;; Fix '<s Tab' not work
;; From: https://emacs.stackexchange.com/questions/46988/why-do-easy-templates-e-g-s-tab-in-org-9-2-not-work
;; The new mechanism is called structured template.
;; The command org-insert-structure-template bound to C-c C-,
;; gives you a list of #+begin_-#+end_ pairs that narrows down while you type and you can use completion.
;; But, you can also get the old easy template system back
(require 'org-tempo)

(add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-switchb)
(setq org-log-done t)

;; Disable auto indent
;; From: https://www.philnewton.net/blog/electric-indent-with-org-mode/
(add-hook 'electric-indent-functions
	  (lambda (x) (when (eq 'org-mode major-mode) 'no-indent)))

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

;; Codes Syntax Highlighting
(require 'ox-latex)
;(add-to-list 'org-latex-packages-alist '("" "listings"))
;(add-to-list 'org-latex-packages-alist '("" "minted"))
;(add-to-list 'org-latex-packages-alist '("" "color"))
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("lualatex -shell-escape -interaction nonstopmode %f"
        "lualatex -shell-escape -interaction nonstopmode %f"))

;; Insert LaTex '\newpage'
(defun wen-org-latex-newpage ()
  (interactive)
  (insert "#+latex: \\newpage\n"))

(defun wen-latex-build-clean()
  (interactive)
  (shell-command "cp -f *.pdf /tmp/ || /bin/true")
  (shell-command "rm -f *.fdb_latexmk *.fls || /bin/true") ;; for 'lsp-latex-build'
  (shell-command "rm -f *.latex || /bin/true")
  (shell-command "rm -f *.pdf || /bin/true")
  (shell-command "rm -f *.txt || /bin/true") ;; plantuml
  (shell-command "rm -f *.aux *.log *.out *.toc *.gz"))

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


;; agenda
(global-set-key (kbd "C-c c")   'org-capture)
(setq org-agenda-files '("~/Documents/PersonalKnowledgeGraph/Daily"))
;; 定义 agenda 文件的位置
(setq org-capture-templates
      `(("w" "Task [work]" entry (file "~/Documents/PersonalKnowledgeGraph/Daily/weekly.org")
         "* TODO %?\nCaptured %<%Y-%m-%d %H:%M>")
        ("p" "Task [person]" entry (file "~/Documents/PersonalKnowledgeGraph/Daily/person_task.org")
         "* TODO %?\nCaptured %<%Y-%m-%d %H:%M>")
        ("n" "Note" entry (file "~/Documents/PersonalKnowledgeGraph/Daily/note.org")
         "* %? :NOTE:\nCaptured %<%Y-%m-%d %H:%M>\n")))

;; 设置移动到的目标文件列表为 agenda-files
;; See link: https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; From: http://0x100.club/wiki_emacs/gtd.html
(defun wen-org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'wen-org-summary-todo)

(setq org-todo-keywords
      '((type "BUG(B!)" "|" "FIXED(F!)" "FEAT(N!)")
        (sequence "TODO(t!)" "DOING(g!)" "|" "DONE(d!)" "BLOCKED(b!)" "REVIEW(r!)" "CANCELED(c @/!)")
        ))

;; 设置关键字的字体颜色
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("DOING" :foreground "yellow" :weight bold)
              ("BLOCKED" :foreground "DarkOrange" :weight bold)
              ("REVIEW" :foreground "orange" :weight bold)
              ("CANCELED" :foreground "gray" :weight bold)
              ("BUG" :foreground "MediumBlue" :weight bold)
              ("FIXED" :foreground "LightGreen" :weight bold)
              ("FEAT" :foreground "azure" :weight bold)
              )))
;; Then each time you turn an entry from a TODO (not-done) state into any of the DONE
;; states, a line ‘CLOSED: [timestamp]’ will be inserted just after the headline.
(setq org-log-done 'time)

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "DOING")

(setq org-clock-out-switch-to-state "DONE")

;; http://linzhichu.github.io/computers/2018/02/28/org-agenda
(setq org-agenda-custom-commands
      (quote (("C" "Simple agenda view"
               ((tags "PRIORITY=\"A\""
                      ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                       (org-agenda-overriding-header "High-priority unfinished tasks:")))
                (agenda "")
                (tags "REFILE"
                      ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                       (org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (alltodo ""
                         ((org-agenda-skip-function
                           '(org-agenda-skip-entry-if 'scheduled))
                          (org-agenda-overriding-header "Global list of Un-scheduled tasks:")
                          ))))

              ("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              )))

;; agenda 里面时间块彩色显示
;; From: https://www.lijigang.com/blog/2018/08/08/%E7%A5%9E%E5%99%A8-org-mode/
;; From: https://emacs-china.org/t/org-agenda/8679/3
(defun ljg/org-agenda-time-grid-spacing ()
  "Set different line spacing w.r.t. time duration."
  (save-excursion
    (let* ((background (alist-get 'background-mode (frame-parameters)))
           (background-dark-p (string= background "dark"))
           (colors (list "#1ABC9C" "#2ECC71" "#3498DB" "#9966ff"))
           pos
           duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
        (goto-char pos)
        (when (and (not (equal pos (point-at-eol)))
                   (setq duration (org-get-at-bol 'duration)))
          (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
            (overlay-put ov 'face `(:background ,(car colors)
                                                :foreground
                                                ,(if background-dark-p "black" "white")))
            (setq colors (cdr colors))
            (overlay-put ov 'line-height line-height)
            (overlay-put ov 'line-spacing (1- line-height))))))))

(add-hook 'org-agenda-finalize-hook #'ljg/org-agenda-time-grid-spacing)


;; 设置生日
;; In order to include entries from the Emacs diary into Org mode's agenda

(setq org-agenda-include-diary t
   diary-file (locate-user-emacs-file "~/Documents/PersonalKnowledgeGraph/Daily/diary.org")
   org-agenda-diary-file 'diary-file)

;; diary for chinese birthday
;; https://emacs-china.org/t/topic/2119/14
(require 'cal-china)
(defun wen-diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
  (if year
      (let* ((calendar-date-style 'american)
             (d-date (diary-make-date lunar-month lunar-day year))
             (a-date (calendar-absolute-from-gregorian d-date))
             (c-date (calendar-chinese-from-absolute a-date))
             (cycle (car c-date))
             (yy (cadr c-date))
             (y (+ (* 100 cycle) yy)))
        (diary-chinese-anniversary lunar-month lunar-day y mark))
    (diary-chinese-anniversary lunar-month lunar-day year mark)))

;; 中国节假日
(require 'cal-china-x)
(setq mark-holidays-in-calendar t)
(setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
(setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
(setq calendar-holidays
   (append cal-china-x-important-holidays
           cal-china-x-general-holidays
           ))

(provide 'core-org)

;;; core-org.el ends here
