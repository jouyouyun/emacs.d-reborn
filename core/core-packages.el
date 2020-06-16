;;; core-packages.el --- Package management configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>
;;

;;; Commentary:
;; This file sets up package repository and define package management functions.

;;; Code:

(require 'cl)
(require 'package)

;; sets up package repositories
;; repository help: https://mirror.tuna.tsinghua.edu.cn/help/elpa/
(setq package-archives '(
                         ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;; set package-user-dir to be relative install path
(setq package-user-dir (expand-file-name "elpa" config-dir))

;; Must initialize package firstly
;; If you don't want it, just comment it.
(package-initialize)

;; define needed installed package list
(defvar preloaded-packages
  '(epl
    use-package
    ace-window
    avy
    anzu
    guru-mode ;; disables some common keybindings and suggests the use of the established Emacs alternatives instead.
    crux ;; a few useful interactive commands. TODO(jouyouyun): learn it.
    editorconfig
    multiple-cursors
    popup-kill-ring
    whole-line-or-region
    expand-region
    easy-kill ;; TODO(jouyouyun): learn it
    gist
    git-timemachine
    gitconfig-mode
    gitignore-mode
    projectile
    smartparens
    smartrep
    undo-tree
    beacon
    which-key
    zenburn-theme
    seethru ;; easily change Emacs' frame transparency
    super-save
    diminish
    diff-hl
    volatile-highlights
    operate-on-number
    htmlize
    ox-gfm
    ox-mediawiki
    org-preview-html
    expand-region
    highlight-parentheses
    zop-to-char)
  "A list of packages to ensure are installed at launch.")

(defun wen-packages-installed-p ()
  "Check if all packages in 'preloaded-packages' are installed."
  (every #'package-installed-p preloaded-packages))

(defun wen-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package preloaded-packages)
    (add-to-list 'preloaded-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun wen-require-packages (packages)
  "Ensure PACKAGES are installed."
  (mapc #'wen-require-package packages))

(define-obsolete-function-alias 'wen-ensure-module-deps 'wen-require-packages)

(defun wen-install-packages ()
  "Install all packages in preloaded-packages."
  (unless (wen-packages-installed-p)
    ;; check for new packages
    (message "%s" "Emacs is now refreshing its packages database...")
    (package-refresh-contents)
    (message "%s" "done.")
    ;; install the missing packages
    (wen-require-packages preloaded-packages)
    ))

;; run package installation
(wen-install-packages)

(defun wen-recompile-init ()
  "Byte-compile all your .el again."
  (interactive)
  (byte-recompile-directory config-dir 0))

(require 'epl)

(defun wen-update ()
  "Update Wen to its latest version."
  (interactive)
  (when (y-or-n-p "Do you want to update Wen? ")
    (message "Updating installed packages...")
    (epl-upgrade)
    (message "Updating Wen...")
    (cd config-dir)
    (shell-command "git pull")
    (wen-recompile-init)
    (message "Update finished. Restart Emacs to complete the process.")))

(defun wen-update-packages ()
  "Update Wen's packages."
  (interactive "P")
  (when (y-or-n-p "Do you want to update packages? ")
    (if arg
        (epl-upgrade)
      (epl-upgrade (cl-remove-if-not (lambda (p) (memq (epl-package-name p) preloaded-packages))
                                     (epl-installed-packages))))
    (message "Update finished. Restart Emacs to complete the process.")))

(defun wen-list-foreign-packages ()
  "Browse third-party packages likes 'package-list-packages'."
  (interactive)
  (package-show-package-list
   (set-difference package-activated-list preloaded-packages)))

(defmacro wen-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar wen-auto-install-alist
  '(("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.cmake\\'" cmake-mode cmake-mode)
    ("CMakeLists\\.txt\\'" cmake-mode cmake-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("Cask" cask-mode cask-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.elm\\'" elm-mode elm-mode)
    ("\\.ex\\'" elixir-mode elixir-mode)
    ("\\.exs\\'" elixir-mode elixir-mode)
    ("\\.elixir\\'" elixir-mode elixir-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.graphql\\'" graphql-mode graphql-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.json\\'" json-mode json-mode)
    ("\\.kt\\'" kotlin-mode kotlin-mode)
    ("\\.kv\\'" kivy-mode kivy-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("\\.proto\\'" protobuf-mode protobuf-mode)
    ("\\.pyd\\'" cython-mode cython-mode)
    ("\\.pyi\\'" cython-mode cython-mode)
    ("\\.pyx\\'" cython-mode cython-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.rs\\'" rust-mode rust-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.styl\\'" stylus-mode stylus-mode)
    ("\\.swift\\'" swift-mode swift-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.thrift\\'" thrift thrift-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)
    ("\\.yaml\\'" yaml-mode yaml-mode)
    ("Dockerfile\\'" dockerfile-mode dockerfile-mode)))


;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (wen-auto-install extension package mode))))
 wen-auto-install-alist)

(provide 'core-packages)

;;; core-packages.el ends here
