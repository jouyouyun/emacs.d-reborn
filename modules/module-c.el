;;; module-c --- C/C++ configurations.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up c/c++.

;;; Code:

(defun wen-c-common-defaults ()
  (setq c-default-style "linux"
        c-basic-offset 4
        tab-width 4
        indent-tabs-mode t)
  (c-set-offset 'substatement-open 1))

(add-hook 'c-mode-common-hook 'wen-c-common-defaults)
;; (add-hook 'c++-mode-hook 'wen-c-common-defaults)

(defun wen-ccls-create-compile-json-cmake (dir)
  (shell-command "cmake -H. -BDebug -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES && ln -s Debug/compile_commands.json .")
  (message "compile_commands.json created by cmake"))

(defun wen-ccls-create-compile-json-makefile (dir)
  (shell-command "make clean && bear make")
  (message "compile_commands.json created by bear make"))

(defun wen-ccls-create-compile-json-ninja (dir)
  (shell-command "ninja -C build -t compdb cxx cc > compile_commands.json")
  (message "compile_commands.json created by ninja"))

(defun wen-ccls-create-or-update-compile-json (bsystem)
  "Set build system for BSYSTEM."
  (interactive "bBuild system? [C]cmake, [M]makefile, [N]ninja:")
  (cond ((eq bsystem "C") (wen-ccls-create-compile-json-cmake (read-directory-name
                                                               "cmake: top of source tree:" default-directory)))
        ((eq bsystem "M") (wen-ccls-create-compile-json-makefile (read-directory-name
                                                                  "makefile: top of source tree:" default-directory)))
        ((eq bsystem "N") (wen-ccls-create-compile-json-ninja (read-directory-name
                                                               "ninja: top of source tree:" default-directory)))
        ))

(defun wen-ccls-create-or-update-ccls ()
  (shell-command "cp ~/.emacs.d/templates/ccls ./.ccls")
  (message "ccls created from templates"))

(provide 'module-c)

;;; module-c.el ends here
