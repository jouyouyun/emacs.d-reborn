;;; core-autoinsert.el --- Auto insert configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;;
;; This file sets up auto insert.

;;; Code:

;; org-mode
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.org\\'" . "Org-mode skeleton")
     '(
       "Short description: "
       "#+OPTIONS: toc:nil num:nil timestamp:nil ^:nil <:nil\n"
       "#+TITLE: Foo\n")))

;; C++
(eval-after-load 'autoinsert
  '(define-auto-insert
     '("\\.\\(CC?\\|cc\\|cxx\\|cpp\\|c++\\)\\'" . "C++ skeleton")
     '("Short description: "
       "/**\n"
       " * Copyright (C) " (format-time-string "%Y") " jouyouyun <jouyouwen717@gmail.com>\n"
       " *\n"
       " * This program is free software; you can redistribute it and/or modify\n"
       " * it under the terms of the GNU General Public License as published by\n"
       " * the Free Software Foundation; either version 3 of the License, or\n"
       " * (at your option) any later version.\n"
       " *\n * "
       (file-name-nondirectory (buffer-file-name))
       " -- " str
       "\n *\n"
       " * Written on " (format-time-string "%A, %e %B %Y.")
       "\n */" > \n \n
       "#include <iostream>" \n \n
       "using namespace std;" \n \n
       "main()" \n
       "{" \n
       > _ \n
       "}" > \n)))

;; C
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.c\\'" . "C skeleton")
     '(
       "Short description: "
       "/**\n"
       " * Copyright (C) " (format-time-string "%Y") " jouyouyun <jouyouwen717@gmail.com>\n"
       " *\n"
       " * This program is free software; you can redistribute it and/or modify\n"
       " * it under the terms of the GNU General Public License as published by\n"
       " * the Free Software Foundation; either version 3 of the License, or\n"
       " * (at your option) any later version.\n"
       " *\n * "
       (file-name-nondirectory (buffer-file-name))
       " -- " str
       "\n *\n"
       " * Written on " (format-time-string "%A, %e %B %Y.")
       "\n */" > \n \n
       "#include <stdio.h>" \n
       "#include \""
       (file-name-sans-extension
        (file-name-nondirectory (buffer-file-name)))
       ".h\"" \n \n
       "int" \n
       "main(int argc, char *argv[])" \n
       "{" > \n
       > _ \n
       "}" > \n)))

;; C/C++ header
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.h\\'" . "C/C++ skeleton")
     '(
       "Short description: "
       "/**\n"
       " * Copyright (C) " (format-time-string "%Y") " jouyouyun <jouyouwen717@gmail.com>\n"
       " *\n"
       " * This program is free software; you can redistribute it and/or modify\n"
       " * it under the terms of the GNU General Public License as published by\n"
       " * the Free Software Foundation; either version 3 of the License, or\n"
       " * (at your option) any later version.\n"
       " *\n * "
       (file-name-nondirectory (buffer-file-name))
       " -- " str
       "\n *\n"
       " * Written on " (format-time-string "%A, %e %B %Y.")
       " */" > \n \n
       "#ifndef __XX_H__" \n
       "#define __XX_H__" \n \n
       "#endif" \n)))

;; ruby
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.rb\\'" . "Ruby skeleton")
     '(
       "Short description: "
       "#!/usr/bin/ruby -w\n"
       "# -*-coding: utf-8 -*-\n\n"
       "###\n"
       " # Copyright (C) " (format-time-string "%Y") " jouyouyun <jouyouwen717@gmail.com>\n"
       " # \n"
       " # This program is free software; you can redistribute it and/or modify\n"
       " # it under the terms of the GNU General Public License as published by\n"
       " # the Free Software Foundation; either version 3 of the License, or\n"
       " # (at your option) any later version.\n"
       " #\n # "
       (file-name-nondirectory (buffer-file-name))
       " -- " str
       "\n #"
       " Written on " (format-time-string "%A, %e %B %Y.")
       "\n###" \n \n)))

;; python
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.py\\'" . "Python skeleton")
     '(
       "Short description: "
       "#!/usr/bin/env python3\n"
       "# -*-coding: utf-8 -*-\n\n"
       "###\n"
       " # Copyright (C) " (format-time-string "%Y") " jouyouyun <jouyouwen717@gmail.com>\n"
       " # \n"
       " # This program is free software; you can redistribute it and/or modify\n"
       " # it under the terms of the GNU General Public License as published by\n"
       " # the Free Software Foundation; either version 3 of the License, or\n"
       " # (at your option) any later version.\n"
       " #\n # "
       (file-name-nondirectory (buffer-file-name))
       " -- " str
       "\n #"
       " Written on " (format-time-string "%A, %e %B %Y.")
       "\n###" \n \n)))

;; shell
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.sh\\'" . "Shell skeleton")
     '(
       "Short description: "
       "#!/usr/bin/env bash\n\n"
       "###\n"
       " # Copyright (C) " (format-time-string "%Y") " jouyouyun <jouyouwen717@gmail.com>\n"
       " # \n"
       " # This program is free software; you can redistribute it and/or modify\n"
       " # it under the terms of the GNU General Public License as published by\n"
       " # the Free Software Foundation; either version 3 of the License, or\n"
       " # (at your option) any later version.\n"
       " #\n # "
       (file-name-nondirectory (buffer-file-name))
       " -- " str
       "\n #"
       " Written on " (format-time-string "%A, %e %B %Y.")
       "\n###" \n \n)))

(provide 'core-autoinsert)

;;; core-autoinsert.el ends here
