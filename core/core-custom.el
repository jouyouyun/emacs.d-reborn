;;; core-custom.el --- Define configuration variables.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;;
;; This file defines some custom variables.

;;; Code:

;; customize
(defgroup wen nil
  "Emacs Wen configuration."
  :prefix "wen-"
  :group 'convenience)

(defcustom wen-auto-save t
  "Non-nil values enable Wen's auto save."
  :type 'boolean
  :group 'wen)

(defcustom wen-guru t
  "Non-nil values enable `guru-mode'."
  :type 'boolean
  :group 'wen)

(defcustom wen-whitespace t
  "Non-nil values enable Wen's whitespace visualization."
  :type 'boolean
  :group 'wen)

(defcustom wen-clean-whitespace-on-save t
  "Cleanup whitespace from file before it's saved.
Will only occur if `wen-whitespace' is also enabled."
  :type 'boolean
  :group 'wen)

(defcustom wen-flyspell t
  "Non-nil values enable Wen's flyspell support."
  :type 'boolean
  :group 'wen)

(defcustom wen-indent-sensitive-modes
  '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list
  :group 'wen)

(defcustom wen-yank-indent-modes '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here."
  :type 'list
  :group 'wen)

(defcustom wen-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'wen)

;; white-sand
(defcustom wen-theme 'github-modern
  "The default color theme, change this in your /personal/preload config."
  :type 'symbol
  :group 'wen)

(defcustom wen-frontend "ivy"
  "The default frontend, avaliables: ivy, helm."
  :type 'string
  :group 'wen)

(defcustom wen-terminal "ansi-term"
  "The default terminal, avaliables: ansi-term, vterm."
  :type 'string
  :group 'wen)

(defcustom wen-knowledge-repo "~/Documents/PersonalKnowledgeGraph"
  "The default personal knowledge graph dir."
  :type 'string
  :group 'wen)

(defcustom wen-daily-sync-repo "~/Documents/daily-sync-repo"
  "The default documents sync repo dir."
  :type 'string
  :group 'wen)

(provide 'core-custom)

;;; core-custom.el ends here
