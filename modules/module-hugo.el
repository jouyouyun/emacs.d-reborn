;;; module-hugo --- Write blog with org-mode by hugo.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This hugo settings.

;;; Code:

(wen-require-package 'ox-hugo)

(use-package ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
  :after ox)

;; At least the following fields need to be exported in org-file header:
;;    HUGO_SECTION
;;    HUGO_BASE_DIR
;;
;; Such as:
;;    #+HUGO_BASE_DIR: ../
;;    #+HUGO_SECTION: post
;;    #+SEQ_TODO: TODO NEXT DRAFT DONE
;;    #+FILETAGS: post
;;    #+OPTIONS:   *:t <:nil timestamp:nil toc:nil ^:{}
;;    #+HUGO_AUTO_SET_LASTMOD: t
;;    #+TITLE: <title>
;;    #+DATE: 2019-11-28T10:38:21+08:00
;;    #+HUGO_TAGS: <tag1> <tag2>
;;    #+HUGO_CATEGORIES: NOTE or BLOG
;;    #+HUGO_DRAFT: false
;;
;; Convert to md:
;;    C-c C-e H h

(defun wen-hugo-auto-insert()
  (interactive)
  (insert "#+HUGO_BASE_DIR: ../
#+HUGO_SECTION: emacs
#+SEQ_TODO: TODO NEXT DRAFT DONE
#+OPTIONS:   *:t <:nil timestamp:nil toc:nil ^:{}
#+HUGO_AUTO_SET_LASTMOD: t
#+DATE: 2022-07-18T11:20:21+08:00
#+HUGO_TAGS: tag1
#+HUGO_CATEGORIES: emacs
#+HUGO_DRAFT: true
"))

(provide 'module-hugo)

;;; module-hugo.el ends here
