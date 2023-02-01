;; Simple emacs configuration for convert '.org' to 'html' or 'tex'
;;
;; Usages:
;;     html : emacs <org file> --batch -l ~/.emacs.d/templates/simple-init.el -f org-html-export-to-html --kill
;;     latex: emacs <org file> --batch -l ~/.emacs.d/templates/simple-init.el -f org-latex-export-to-latex --kill

;; Always load newest byte code
;; Non-nil means load prefers the newest version of a file.
(setq load-prefer-newer t)

(defvar current-home
  (getenv "HOME"))

;; Sets up the modules load path and other configuration files.
(defvar config-dir (expand-file-name ".emacs.d" current-home)
  "Emacs configuration root dir.")
(defvar config-pkgs-dir (expand-file-name "elpa" config-dir)
  "Emacs elpa packages dir.")

;; add configuration's directories to `load-path'
(add-to-list 'load-path config-pkgs-dir)

(require 'cl-lib)
(require 'package)
;; set package-user-dir to be relative install path
(setq package-user-dir (expand-file-name "elpa" config-dir))

;; Must initialize package firstly
;; If you don't want it, just comment it.
(package-initialize)

(add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))

(advice-add 'sh-set-shell :around
            (lambda (orig-fun &rest args)
              (cl-letf (((symbol-function 'message) #'ignore))
                (apply orig-fun args))))

(require 'htmlize)


(defun wen-c-common-defaults ()
  (setq c-default-style "linux"
        c-basic-offset 8
        tab-width 8
        indent-tabs-mode t)
  (c-set-offset 'substatement-open 1))

(add-hook 'c-mode-common-hook 'wen-c-common-defaults)
(add-hook 'c++-mode-hook 'wen-c-common-defaults)

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
