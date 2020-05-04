;;; core-ivy.el --- Ivy configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;;
;; This file sets up ivy, swiper and counsel.

;;; Code:
(wen-require-packages '(ivy swiper counsel all-the-icons-ivy counsel-tramp counsel-projectile lsp-ivy))

(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)

;; enable this if you want `swiper' to use it
(setq search-default-mode #'char-fold-to-regexp)
;;; (global-set-key "C-s" 'swiper)

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(use-package all-the-icons-ivy
             :ensure t
             :config
             (all-the-icons-ivy-setup))

;; counsel-tramp for sudo, ssh, docker
(setq tramp-default-method "ssh")
(define-key global-map (kbd "C-c s") 'counsel-tramp)
;; speed up tramp
(add-hook 'counsel-tramp-pre-command-hook '(lambda () (global-aggressive-indent-mode 0)
                                             (projectile-mode 0)
                                             (editorconfig-mode 0)))
(add-hook 'counsel-tramp-quit-hook '(lambda () (global-aggressive-indent-mode 1)
                                      (projectile-mode 1)
                                      (editorconfig-mode 1)))
(setq make-backup-files nil)
(setq create-lockfiles nil)
;; If you are using docker-tramp, docker is also supplemented.
;; If you are using vagrant-tramp, vagrant is also supplemented.

;; counsel-projectile
;; project management
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(provide 'core-ivy)

;;; core-ivy.el ends here
