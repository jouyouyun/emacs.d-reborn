;;; module-c --- C/C++ configurations.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up c/c++.

;;; Code:

(defun wen-c-common-defaults ()
  (interactive)
  (setq c-default-style "linux"
        ;; c-basic-offset 4
        ;; tab-width 4
        ;; indent-tabs-mode t
        ;; for tab backward hitting from 4->1
        ;; backward-delete-char-untabify-method nil
        )
  ;; (c-set-offset 'substatement-open 1)
  )

(defun wen-cpp-common-defaults ()
  (interactive)
  (setq cpp-default-style "linux"
        ;; tab-width 4
        ;; indent-tabs-mode t
        ;; cpp-basic-offset 4
        ;; for tab backward hitting from 4->1
        ;; backward-delete-char-untabify-method nil
        )
  ;; (c-set-offset 'substatement-open 1)
  )

(add-hook 'c-mode-common-hook 'wen-c-common-defaults)
(add-hook 'c-mode-hook 'wen-c-common-defaults)
(add-hook 'c++-mode-hook 'wen-cpp-common-defaults)

(defun wen-ccls-create-compile-json-cmake (dir)
  (let ((default-directory dir))
    (shell-command "cmake -H. -Bbuild -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES"))
  (let ((default-directory dir))
    (shell-command "ln -svf Build/compile_commands.json ."))
  (message "compile_commands.json created by cmake"))

(defun wen-ccls-create-compile-json-makefile (dir)
  (let ((default-directory dir))
    (shell-command "make clean && bear -- make -j16"))
  (message "compile_commands.json created by bear make"))

(defun wen-ccls-create-compile-json-ninja (dir)
  (let ((default-directory dir))
    (shell-command "ninja -C build -t compdb cxx cc > compile_commands.json"))
  (message "compile_commands.json created by ninja"))

(defun wen-ccls-create-compile-json-meson (dir)
  (let ((default-directory dir))
    (shell-command "meson build"))
  (message "compile_commands.json created by meson"))

(defun wen-ccls-create-or-update-compile-json (build-type)
  "Set build system for BSYSTEM."
  (interactive "sbuild type [C]cmake, [B]makefile, [N]ninja, [M]meson:")
  (message "Build type: %s" build-type)
  (cond ((string= build-type "C") (wen-ccls-create-compile-json-cmake (read-directory-name
                                                               "cmake: top of source tree:" default-directory)))
        ((string= build-type "B") (wen-ccls-create-compile-json-makefile (read-directory-name
                                                                  "makefile: top of source tree:" default-directory)))
        ((string= build-type "N") (wen-ccls-create-compile-json-ninja (read-directory-name
                                                               "ninja: top of source tree:" default-directory)))
        ((string= build-type "M") (wen-ccls-create-compile-json-meson (read-directory-name
                                                                       "meson: top of source tree:" default-directory)))
        ))

(defun wen-ccls-copy-ccls (dir)
  (let ((default-directory dir))
    (shell-command "cp -f ~/.emacs.d/templates/ccls .ccls"))
  (message "ccls created from templates"))

(defun wen-ccls-create-or-update-ccls ()
  (interactive)
  (wen-ccls-copy-ccls (read-directory-name
                       "ccls: top of source tree:" default-directory)))

(provide 'module-c)

;;; module-c.el ends here
