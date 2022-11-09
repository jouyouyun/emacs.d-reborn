;;; module-custom.el --- Define configuration variables.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;;
;; This file defines some custom variables for modules.

;;; Code:

(defgroup wen-module nil
  "Emacs Wen module configuration."
  :prefix "wen-module-"
  :group 'convenience)

(defcustom wen-module-cpp-server "clangd"
  "The default cpp lsp server, availables: clangd, ccls."
  :type 'string
  :group 'wen-module)

(defcustom wen-module-lsp-frame "lsp"
  "The default lsp frame, availables: lsp, lsp-bridge."
  :type 'string
  :group 'wen-module)

(defcustom wen-module-tex-server "texlab"
  "The default latex server, avaliables: digestif, texlab."
  :type 'string
  :group 'wen-module)

(defcustom wen-module-caldav-provider "google"
  "The default caldav provider, avaliables: google, dingtalk."
  :type 'string
  :group 'wen-module)

(defcustom wen-module-caldav-username "jouyouwen717@gmail.com"
  "The caldav username."
  :type 'string
  :group 'wen-module)

(defcustom wen-module-caldav-client-id ""
  "The caldav client id."
  :type 'string
  :group 'wen-module)

(defcustom wen-module-caldav-client-secret ""
  "The caldav client secret."
  :type 'string
  :group 'wen-module)

(provide 'module-custom)

;;; module-custom.el ends here
