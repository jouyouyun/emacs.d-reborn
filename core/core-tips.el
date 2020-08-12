;;; core-tips.el --- Keymap usage tips.
;;
;; Author: jouyouyunyun <jouyouwen717@gmail.com>


;;; Commentary:
;;
;; This file sets up for tips

;;; Code:

(defvar wen-tips
  '("Press <C-c o> to open a file with external program."
    "Press <C-c p f> to navigate a project's files with ido."
    "Press <s-r> to open a recently visited file."
    "Press <C-c p s g> to run grep on a project."
    "Press <C-c p p> to switch between projects."
    "Press <C-=> to expand the selected region."
    "Press <C-c g> to search in Google."
    "Press <C-c G> to search in GitHub."
    "Press <C-c y> to search in YouTube."
    "Press <C-c U> to search in DuckDuckGo."
    "Press <C-c r> to rename the current buffer and the file it's visiting if any."
    "Press <C-c t> to open a terminal in Emacs."
    "Press <C-c k> to kill all the buffers, but the active one."
    "Press <C-x g> to run magit-status."
    "Press <C-c D> to delete the current file and buffer."
    "Press <C-c s> to swap two windows."
    "Press <S-RET> or <M-o> to open a line beneath the current one."
    "Press <s-o> to open a line above the current one."
    "Press <C-c C-z> in a Elisp buffer to launch an interactive Elisp shell."
    "Press <C-Backspace> to kill a line backwards."
    "Press <C-S-Backspace> or <s-k> to kill the whole line."
    "Press <s-j> or <C-^> to join lines."
    "Press <s-.> or <C-c j> to jump to the start of a word in any visible window."
    "Press <f11> to toggle fullscreen mode."
    "Press <f12> to toggle the menu bar."
    "Press <C-c p h> to navigate a project in Helm."
    "Explore the Tools->Wen menu to find out about some of Wen extensions to Emacs."
    "Access the official Emacs manual by pressing <C-h r>."))

(defun wen-tip-of-the-day ()
  "Display a random entry from `wen-tips'."
  (interactive)
  (when (and wen-tips (not (window-minibuffer-p)))
    ;; pick a new random seed
    (random t)
    (message
     (concat "Wen tip: " (nth (random (length wen-tips)) wen-tips)))))

(defun wen-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(provide 'core-tips)

;;; core-tips.el ends here
