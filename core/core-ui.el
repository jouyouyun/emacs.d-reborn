;;; core-ui.el --- UI tweaks.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;; This file sets up tool bar, theme, opacity etc.

;;; Code:

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
;; (when (fboundp 'tool-bar-mode)
;;   (tool-bar-mode -1))

;; hide menu bar
;; (menu-bar-mode -1)
;; Disable menubar, toolbar and scrollbar
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; Disable menubar, toolbar and scrollbar
;; (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
;;   (when (fboundp mode) (funcall mode -1)))

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " Wen - " (:eval (if (buffer-file-name)
                                                    (abbreviate-file-name (buffer-file-name))
                                              "%b"))))

;; use zenburn as the default theme
(when wen-theme
  (load-theme wen-theme t))

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; show the cursor when moving after big movements in the window
(require 'beacon)
(beacon-mode +1)

;; show available keybindings after you start typing
(require 'which-key)
(which-key-mode +1)

(defun wen-fullscreen ()
  "Make Emacs window fullscreen."
  (interactive)
  (if (eq window-system 'x)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0))
    (error "Only X server is supported")))

;; change frame opacity
(require 'seethru)
;; "C-c 8" and "C-c 9"
(seethru-recommended-keybinds "C-c")
(global-set-key (kbd "C-c 0")
				(lambda () (interactive) (seethru 100)))
;; hold control while wheeling mouse to change transparency
(seethru-mouse-bindings "C")

;; Compilation from Emacs
(defun wen-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(require 'compile)
(setq compilation-ask-about-save nil  ; Just save before compiling
      compilation-always-kill t       ; Just kill old compile processes before
                                        ; starting the new one
      compilation-scroll-output 'first-error ; Automatically scroll to first
                                        ; error
      )

;; Colorize output of Compilation Mode, see
;; http://stackoverflow.com/a/3072831/355252
(require 'ansi-color)
(add-hook 'compilation-filter-hook #'wen-colorize-compilation-buffer)

(provide 'core-ui)

;;; core-ui.el ends here
