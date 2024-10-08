;;; core-packages.el --- Package management configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>
;;

;;; Commentary:
;; This file sets up package repository and define package management functions.

;;; Code:

(require 'cl-lib)
(require 'package)

;; sets up package repositories
;; repository help: https://mirror.tuna.tsinghua.edu.cn/help/elpa/
(setq package-archives '(
                         ("gnu"      . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org"      . "https://orgmode.org/elpa/")
                         ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")))
;; set priority
(setq package-archive-priorities '(("melpa"    . 5)
                                   ("jcs-elpa" . 0)))

;; https://github.com/d12frosted/elpa-mirror
;; (setq package-archives
;;       '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
;;         ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
;;         ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))

;; set package-user-dir to be relative install path
(setq package-user-dir (expand-file-name "elpa" config-dir))

;; fix: Package ‘spinner-1.7.3’ is unavailable
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Must initialize package firstly
;; If you don't want it, just comment it.
(package-initialize)

;; define needed installed package list
(defvar preloaded-packages
  '(epl
    use-package
    gnu-elpa-keyring-update ;; updates the GPG keys used by the ELPA package manager
    ace-window
    ag
    avy
    ;;marginalia
    ;;embark
    ;;embark-consult
    anzu
    guru-mode ;; disables some common keybindings and suggests the use of the established Emacs alternatives instead.
    crux ;; a few useful interactive commands. TODO(jouyouyun): learn it.
    editorconfig
    multiple-cursors
    ace-popup-menu
    browse-kill-ring
    popup
    whole-line-or-region
    expand-region
    easy-kill ;; TODO(jouyouyun): learn it
    gist
    git-timemachine
    gitconfig
    projectile
    smartparens
    smartrep
    undo-tree
    beacon
    which-key
    zenburn-theme
    white-sand-theme
    github-modern-theme
    seethru ;; easily change Emacs' frame transparency
    super-save
    diminish
    diff-hl
    volatile-highlights
    operate-on-number
    htmlize
    gh-md ;; Render markdown using the Github api
    ox-gfm
    ox-mediawiki
    org-preview-html
    expand-region
    highlight-parentheses
    cal-china-x
    pdf-tools
    yasnippet
    smart-tab
    neotree
    rainbow-mode
    rainbow-delimiters
    mwim
    winum
    ;; persp-mode
    treemacs
    treemacs-projectile
    ;; treemacs-persp
    ;; flycheck
    xmind-org ;; import xmind file into org mode
    zop-to-char)
  "A list of packages to ensure are installed at launch.")

(defun wen-packages-installed-p ()
  "Check if all packages in 'preloaded-packages' are installed."
  (cl-every #'package-installed-p preloaded-packages))

(defun wen-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package preloaded-packages)
    (add-to-list 'preloaded-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun wen-require-packages (packages)
  "Ensure PACKAGES are installed."
  (mapc #'wen-require-package packages))

;;; Fix byte compile error. The third argument WHEN is mandatory since https://github.com/emacs-mirror/emacs/commit/32c6732d16385f242b1109517f25e9aefd6caa5c
;;; From https://github.com/emacs-lsp/lsp-mode/pull/2498
(define-obsolete-function-alias 'wen-ensure-module-deps 'wen-require-packages "wen packages 1.1")

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

(defun wen-update-packages (&optional arg)
  "Update Wen's packages. his includes package installed via `wen-require-package'."
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
  '(("\\.asn1\\'" asn1-mode asn1-mode)
    ("\\.clj\\'" clojure-mode clojure-mode)
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
