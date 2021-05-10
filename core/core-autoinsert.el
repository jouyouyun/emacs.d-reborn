;;; core-autoinsert.el --- Auto insert configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;;
;; This file sets up auto insert.

;;; Code:

;; latex
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.tex\\'" . "LaTex skeleton")
     '("Short description: "
       "% Option Settings: https://www.latexstudio.net/index/details/index/mid/683.html\n"
       "\\documentclass[a4paper,12pt]{ctexart}\n"
       "\\usepackage{booktabs}\n"
       "\\usepackage{indentfirst}\n"
       "% to include pdf/eps/png files\n"
       "\\usepackage{graphicx}\n"
       "% useful to add 'todo' markers\n"
       "\\usepackage{todonotes}\n"
       "\\setlength{\\marginparwidth}{2cm} % fix todonotes warning\n"
       "% hyperrefs\n"
       "\\usepackage{hyperref}\n"
       "% ----------------- Code blocks ----------------\n"
       "% Dependencies: pip install pygments\n"
       "% nice source code formatting\n"
       "\\usepackage{xcolor}\n"
       "\\definecolor{bg}{rgb}{0.98,0.98,0.98}\n"
       "\\usepackage[cache=false]{minted}\n"
       "\\setminted{\n"
       "  fontsize=\\small,\n"
       "  baselinestretch=1,\n"
       "  xleftmargin=4em,\n"
       "  breaklines,\n"
       "  mathescape,\n"
       "  linenos,\n"
       "  numbersep=5pt,\n"
       "  frame=leftline,\n"
       "  framesep=2mm,\n"
       "  autogobble,\n"
       "  style=tango,\n"
       "  bgcolor=bg\n"
       "}\n"
       "% ----------------- Chinese ----------------\n"
       "\\usepackage{ctex}\n"
       "% change style of section headings\n"
       "\\usepackage{sectsty}\n"
       "\\allsectionsfont{\\sffamily}\n"
       "% only required for orgmode ticked TODO items, can remove\n"
       "\\usepackage{amssymb}\n"
       "% only required for underlining text\n"
       "\\usepackage[normalem]{ulem}\n"
       "% often use this in differential operators:\n"
       "\\renewcommand{\\d}{\\ensuremath{\\mathrm{d}}}\n"
       "% allow more reasonable text width for most documents than LaTeX default\n"
       "\\setlength{\\textheight}{21cm}\n"
       "\\setlength{\\textwidth}{16cm}\n"
       "% reduce left and right margins accordingly\n"
       "\\setlength{\\evensidemargin}{-0cm}\n"
       "\\setlength{\\oddsidemargin}{-0cm}\n"
       "% reduce top margin\n"
       "\\setlength{\\topmargin}{0cm}\n"
       "% references formats\n"
       "\\usepackage[round]{natbib}\n"
       "% Increase default line spacing a little\n"
       "\\usepackage{setspace}\n"
       "\\renewcommand{\\baselinestretch}{1.5}\n"
       "\\setlength{\\parskip}{0.8em}\n"
       "% Line & paragraph space end\n"
       "% item list margin\n"
       "\\usepackage{enumitem}\n"
       "% Breaking Page Between Title and Toc\n"
       "\\makeatletter \\def\\@maketitle{\\null \\begin{center} {\\vskip 5em \\Huge \\@title} \\vskip 30em {\\LARGE \\@author} \\vskip 3em {\\LARGE \\@date} \\end{center} \\newpage} \\makeatother\n"
       "% End of Breaking Page Between Title and Toc\n"
       "% UML, see: https://github.com/koppor/plantuml\n"
       "\\usepackage{plantuml} % MUST Export PLANTUML_JAR\n"
       "\\renewcommand\\contentsname{目录}\n"
       "% Generate Tex File: C-c C-e l l; then replace verbatim with minted, and must special the code language\n"
       "% Generate PDF: lualatex -shell-escape -interaction nonstopmode <tex file>\n"
       "\n\\title{}\n"
       "\\author{}\n"
       "\\date{}\n"
       "\n\\begin{document}\n"
       "\n\\maketitle\n"
       "\\tableofcontents\n"
       "\\newpage\n"
       "\n\\end{document}\n"
)))

;; org-mode
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.org\\'" . "Org-mode skeleton")
     '(
       "Short description: "
       "# % Options Settings: https://orgmode.org/manual/Export-Settings.html\n"
       "#+OPTIONS: timestamp:nil ^:nil <:nil p:t prop:t tags:t tasks:t todo:t\n"
       "#+LATEX_CLASS: article\n"
       "#+LaTeX_CLASS_OPTIONS: [a4paper,12pt]\n"
       "#+LATEX_HEADER: \\usepackage{booktabs}\n"
       "# % to include pdf/eps/png files\n"
       "#+LATEX_HEADER: \\usepackage{indentfirst}\n"
       "#+LATEX_HEADER: \\usepackage{graphicx}\n"
       "# % useful to add 'todo' markers\n"
       "#+LaTeX_HEADER: \\usepackage{todonotes}\n"
       "#+LaTeX_HEADER: \\setlength{\\marginparwidth}{2cm} % fix todonotes warning\n"
       "# % hyperrefs\n"
       "#+LaTeX_HEADER: \\usepackage{hyperref}\n"
       "# % ----------------- Code blocks ----------------\n"
       "# % Dependencies: pip install pygments\n"
       "# % nice source code formatting\n"
       "#+LaTeX_HEADER: \\usepackage{xcolor}\n"
       "#+LaTeX_HEADER: \\definecolor{bg}{rgb}{0.98,0.98,0.98}\n"
       "#+LaTeX_HEADER: \\usepackage[cache=false]{minted}\n"
       "#+LaTeX_HEADER: \\setminted{\n"
       "#+LaTeX_HEADER:   fontsize=\\small,\n"
       "#+LaTeX_HEADER:   baselinestretch=1,\n"
       "#+LaTeX_HEADER:   xleftmargin=4em,\n"
       "#+LaTeX_HEADER:   breaklines,\n"
       "#+LaTeX_HEADER:   mathescape,\n"
       "#+LaTeX_HEADER:   linenos,\n"
       "#+LaTeX_HEADER:   numbersep=5pt,\n"
       "#+LaTeX_HEADER:   frame=leftline,\n"
       "#+LaTeX_HEADER:   framesep=2mm,\n"
       "#+LaTeX_HEADER:   autogobble,\n"
       "#+LaTeX_HEADER:   style=tango,\n"
       "#+LaTeX_HEADER:   bgcolor=bg\n"
       "#+LaTeX_HEADER: }\n"
       "# % change style of section headings\n"
       "#+LaTeX_HEADER: \\usepackage{sectsty}\n"
       "#+LaTeX_HEADER: \\allsectionsfont{\\sffamily}\n"
       "# % only required for orgmode ticked TODO items, can remove\n"
       "#+LaTeX_HEADER: \\usepackage{amssymb}\n"
       "# % only required for underlining text\n"
       "#+LaTeX_HEADER: \\usepackage[normalem]{ulem}\n"
       "# % often use this in differential operators:\n"
       "#+LaTeX_HEADER: \\renewcommand{\\d}{\\ensuremath{\\mathrm{d}}}\n"
       "# % allow more reasonable text width for most documents than LaTeX default\n"
       "#+LaTeX_HEADER: \\setlength{\\textheight}{21cm}\n"
       "#+LaTeX_HEADER: \\setlength{\\textwidth}{16cm}\n"
       "# % reduce left and right margins accordingly\n"
       "#+LaTeX_HEADER: \\setlength{\\evensidemargin}{-0cm}\n"
       "#+LaTeX_HEADER: \\setlength{\\oddsidemargin}{-0cm}\n"
       "# % reduce top margin\n"
       "#+LaTeX_HEADER: \\setlength{\\topmargin}{0cm}\n"
       "# % references formats\n"
       "#+LaTeX_HEADER: \\usepackage[round]{natbib}\n"
       "# % Chinese supported\n"
       "#+LATEX_HEADER: \\usepackage{ctex}\n"
       "# % Increase default line spacing a little\n"
       "#+LATEX_HEADER: \\usepackage{setspace}\n"
       "#+LATEX_HEADER: \\renewcommand{\\baselinestretch}{1.5}\n"
       "#+LATEX_HEADER: \\setlength{\\parskip}{0.8em}\n"
       "# % Line & paragraph space end\n"
       "# % item list margin\n"
       "#+LATEX_HEADER: \\usepackage{enumitem}\n"
       "# % Breaking Page Between Title and Toc\n"
       "#+LATEX_HEADER: \\makeatletter \\def\\@maketitle{\\null \\begin{center} {\\vskip 5em \\Huge \\@title} \\vskip 30em {\\LARGE \\@author} \\vskip 3em {\\LARGE \\@date} \\end{center} \\newpage} \\makeatother\n"
       "# % End of Breaking Page Between Title and Toc\n"
       "#+LATEX_HEADER: \\renewcommand\\contentsname{目录}\n"
       "# Generate Tex File: C-c C-e l l; then replace verbatim with minted, and must special the code language\n"
       "#+LATEX_HEADER: % Generate PDF: xelatex -shell-escape <tex file>\n"
       "#+AUTHOR: jouyouyun\n"
       "#+EMAIL: yanbowen717@gmail.com\n"
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
