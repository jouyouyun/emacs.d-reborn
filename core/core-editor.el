;;; core-editor.el --- Editor configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;;
;; This file sets up editor.

;;; Code:

;; use settings from .editorconfig file when present
(require 'editorconfig)
(editorconfig-mode 1)

;; (use-package flycheck
;; :init (global-flycheck-mode))

;; auto break line
(global-visual-line-mode 1)

;; smart move
(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;; avy
(require 'avy)
(global-set-key (kbd "M-g c") 'avy-goto-char)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g f") 'avy-goto-line)

;;(use-package marginalia
;;  :ensure t
;;  :config
;;  (marginalia-mode))
;;
;;(use-package embark
;;  :ensure t
;;
;;  :bind
;;  (("C-." . embark-act)         ;; pick some comfortable binding
;;   ("C-c M-;" . embark-dwim)        ;; good alternative: M-.
;;   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
;;
;;  :init
;;
;;  ;; Optionally replace the key help with a completing-read interface
;;  (setq prefix-help-command #'embark-prefix-help-command)
;;
;;  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
;;  ;; strategy, if you want to see the documentation from multiple providers.
;;  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
;;  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
;;
;;  :config
;;
;;  ;; Hide the mode line of the Embark live/completions buffers
;;  (add-to-list 'display-buffer-alist
;;               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                 nil
;;                 (window-parameters (mode-line-format . none)))))
;;
;;;; Consult users will also want the embark-consult package.
;;(use-package embark-consult
;;  :ensure t ; only need to install it, embark loads it after consult if found
;;  :hook
;;  (embark-collect-mode . consult-preview-at-point-mode))

;; anzu
(require 'anzu)
(global-anzu-mode +1)
;; enable anzu minor mode
;; (anzu-mode +1)
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

;; anzu-query-replace-at-cursor for all file
;;anzu-query-replace-at-cursor-thing for this function or segment region

(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; multi-cursor
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-s") 'mc/skip-to-next-like-this)

(require 'ace-popup-menu)
(ace-popup-menu-mode 1)
(setq ace-popup-menu-show-pane-header t)

;; browse-kill-ring
(require 'browse-kill-ring)
(global-set-key (kbd "M-y") 'browse-kill-ring)

(require 'whole-line-or-region)
;; Comment or uncomment
;; (global-set-key (kbd "M-;") 'whole-line-or-region-comment-dwim-2)
(global-set-key (kbd "M-w") 'whole-line-or-region-copy-region-as-kill)
;; 当光标放在行的始端，或者行的中间位置，即为注释该行代码
;; 当光标放在行的末端，即为给该行代码添加注释
(defun improve-comment-dwim-line (&optional arg)
  "Replacement ARG for the 'comment-dwim' command."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key (kbd "M-;") 'improve-comment-dwim-line)

(setq create-lockfiles nil)

;; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
;; Two callable functions for enabling/disabling tabs in Emacs
(defun wen-disable-tabs ()
  (interactive)
  (setq indent-tabs-mode nil))

(defun wen-enable-tabs ()
  (interactive)
  ;; (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  )

;; default tab settings
(setq-default indent-tabs-mode nil
              tab-width 4
              inhibit-splash-screen t
              initial-scratch-message nil
              sentence-end-double-space nil
              ;; disable indent for previous line
              electric-indent-inhibit t
              ;; delete tab method
              backward-delete-char-untabify-method nil
              ;; backward-delete-char-untabify-method 'hungry
              )

;; set tab color
(global-whitespace-mode)
(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#9999CC")))))

(setq whitespace-display-mappings
      '((tab-mark 9 [124 9] [92 9])))

(use-package smart-tab
  :config
  (progn
    (defun @-enable-smart-tab ()
      (smart-tab-mode))
    (add-hook 'prog-mode-hook '@-enable-smart-tab)
    ))

;; Newline at end of file
;; (setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; autosave the undo-tree history
(use-package undo-tree
  :ensure t
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    ))
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
;; (setq undo-tree-auto-save-history t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; hippie-expand replace dabbbrev-expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; smart paring for all
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)
(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'maxima-mode-hook
               'ielm-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'php-mode-hook
               'python-mode-hook
               'js-mode-hook
               'go-mode-hook
               'qml-mode-hook
               'jade-mode-hook
               'css-mode-hook
               'ruby-mode-hook
               'coffee-mode-hook
               'rust-mode-hook
               'qmake-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               'minibuffer-inactive-mode-hook
               ))
  (add-hook hook '(lambda () (smartparens-mode 1))))

;; highlight parentheses
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; disable annoying blink-matching-paren
(setq blink-matching-paren t)

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" config-savefile-dir))
;; activate it for all buffers
(save-place-mode 1)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(kill-ring
        search-ring
        regexp-search-ring))
;; save every minute
(setq savehist-autosave-interval 60)
;; keep the home clean
(setq savehist-file (expand-file-name "savehist" config-savefile-dir))
(savehist-mode +1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" config-savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)

(defun wen-recentf-exclude-p (file)
  "A predicate to decide whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (cl-some (lambda (dir)
               (string-prefix-p dir file-dir))
             (mapcar 'file-truename (list config-savefile-dir package-user-dir)))))

(add-to-list 'recentf-exclude 'wen-recentf-exclude-p)

(recentf-mode +1)

;; highlight the current line
(global-hl-line-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; note - this should be after volatile-highlights is required
;; add the ability to cut the current line, without marking it
(require 'rect)
(require 'crux)
(crux-with-region-or-line kill-region)

;; flyspell-mode does spell-checking on the fly as you type
;; TODO(jouyouyun): install 'aspell-en'
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
;; use American English as ispell default dictionary to solve 'zh_CN'
(ispell-change-dictionary "american" t)

(defun wen-enable-flyspell ()
  "Enable command `flyspell-mode' if `wen-flyspell' is not nil."
  (when (and wen-flyspell (executable-find ispell-program-name))
    (flyspell-mode +1)))

(defun wen-cleanup-maybe ()
  "Invoke `whitespace-cleanup' if `wen-clean-whitespace-on-save' is not nil."
  (when wen-clean-whitespace-on-save
    (whitespace-cleanup)))

(defun wen-enable-whitespace ()
  "Enable `whitespace-mode' if `wen-whitespace' is not nil."
  (when wen-whitespace
    ;; keep the whitespace decent all the time (in this buffer)
    (add-hook 'before-save-hook 'wen-cleanup-maybe nil t)
    (whitespace-mode +1)))

(add-hook 'text-mode-hook 'wen-enable-flyspell)
(add-hook 'text-mode-hook 'wen-enable-whitespace)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(require 'expand-region)

;; bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" config-savefile-dir)
      bookmark-save-flag 1)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)

;; clean up obsolete buffers automatically
(require 'midnight)

(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))

(require 'tabify)
(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

;; automatically indenting yanked text if in programming-modes
(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) wen-yank-indent-threshold)
      (indent-region beg end nil)))

(defmacro advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.

The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))

(advise-commands "indent" (yank yank-pop) after
  "If current mode is one of `wen-yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode wen-indent-sensitive-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode wen-yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; whitespace-mode config
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" config-savefile-dir))

;; sensible undo
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; enable winner-mode to manage window configurations
;; C-c left --> winner-undo
;; C-c right --> winner-redo
(winner-mode +1)

;; diff-hl
(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

;; easy-kill
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

;; operate-on-number
(require 'operate-on-number)
(require 'smartrep)

(smartrep-define-key global-map "C-c ."
  '(("+" . apply-operation-to-number-at-point)
    ("-" . apply-operation-to-number-at-point)
    ("*" . apply-operation-to-number-at-point)
    ("/" . apply-operation-to-number-at-point)
    ("\\" . apply-operation-to-number-at-point)
    ("^" . apply-operation-to-number-at-point)
    ("<" . apply-operation-to-number-at-point)
    (">" . apply-operation-to-number-at-point)
    ("#" . apply-operation-to-number-at-point)
    ("%" . apply-operation-to-number-at-point)
    ("'" . operate-on-number-at-point)))

(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "Open file with emacsclient with cursors positioned on requested line.
Most of console-based utilities prints filename in format
'filename:linenumber'.  So you may wish to open filename in that format.
Just call:

  emacsclient filename:linenumber

and file 'filename' will be opened and cursor set on line 'linenumber'"
  (ad-set-arg 0
              (mapcar (lambda (fn)
                        (let ((name (car fn)))
                          (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                              (cons
                               (match-string 1 name)
                               (cons (string-to-number (match-string 2 name))
                                     (string-to-number (or (match-string 3 name) ""))))
                            fn))) files)))

;;; search

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; search selected text
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

(use-package neotree
  :custom
  (neo-theme 'nerd2)
  :config
  (progn
    (setq neo-smart-open t)
    (setq neo-theme (if (display-graphic-p) 'icons 'nerd))
    (setq neo-window-fixed-size nil)
    ;; (setq-default neo-show-hidden-files nil)
    ;; (global-set-key [f2] 'neotree-toggle)
    ;; (global-set-key [f8] 'neotree-dir)
  ))

(if (not (display-graphic-p))
    (progn
      ;; 增大垃圾回收的阈值，提高整体性能（内存换效率）
      (setq gc-cons-threshold (* 8192 8192))
      ;; 增大同LSP服务器交互时的读取文件的大小
      (setq read-process-output-max (* 1024 1024 128)) ;; 128MB
      ))

;; From: https://huadeyu.tech/tools/emacs-setup-notes.html
(use-package rainbow-mode
  :config
  (progn
    (defun @-enable-rainbow ()
      (rainbow-mode t))
    (add-hook 'prog-mode-hook '@-enable-rainbow)
    ))
(use-package rainbow-delimiters
  :config
  (progn
    (defun @-enable-rainbow-delimiters ()
      (rainbow-delimiters-mode t))
    (add-hook 'prog-mode-hook '@-enable-rainbow-delimiters)))
(if (display-graphic-p)
    (progn
      (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                     (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                     (36 . ".\\(?:>\\)")
                     (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                     (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                     (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                     (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                     (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                     (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                     (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                     (48 . ".\\(?:x[a-zA-Z]\\)")
                     (58 . ".\\(?:::\\|[:=]\\)")
                     (59 . ".\\(?:;;\\|;\\)")
                     (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                     (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                     (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                     (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                     (91 . ".\\(?:]\\)")
                     (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                     (94 . ".\\(?:=\\)")
                     (119 . ".\\(?:ww\\)")
                     (123 . ".\\(?:-\\)")
                     (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                     (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                     )
                   ))
        (dolist (char-regexp alist)
          (set-char-table-range composition-function-table (car char-regexp)
                                `([,(cdr char-regexp) 0 font-shape-gstring]))))
      ))

;; large file
(wen-require-package 'vlf)
(require 'vlf-setup)

(provide 'core-editor)

;;; core-editor.el ends here
