;;; core-chinese.el --- Chinese configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;;
;; This file sets up chinese.

;;; Code:

(wen-require-package 'pyim)
(wen-require-package 'pyim-basedict)

(require 'pyim)
(require 'pyim-basedict)
(require 'pyim-cregexp-utils)

(pyim-basedict-enable)

;; 我使用全拼
(pyim-default-scheme 'quanpin)
;; (setq pyim-default-scheme 'pyim-shuangpin)
;; 云拼音
(setq pyim-cloudim 'baidu)

;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;; 我自己使用的中英文动态切换规则是：
;; 1. 光标只有在注释里面时，才可以输入中文。
;; 2. 光标前是汉字字符时，才能输入中文。
;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
;; (setq-default pyim-english-input-switch-functions
;;               '(pyim-probe-dynamic-english
;;                 pyim-probe-isearch-mode
;;                 pyim-probe-program-mode
;;                 pyim-probe-org-structure-template))

;; (setq-default pyim-punctuation-half-width-functions
;;               '(pyim-probe-punctuation-line-beginning
;;                 pyim-probe-punctuation-after-punctuation))

;; 开启拼音搜索功能
;; (pyim-isearch-mode 1)

;; 使用 pupup-el 来绘制选词框
(require 'popup nil t)
(setq pyim-page-tooltip 'popup)

;; 如果使用 pyim-dregcache dcache 后端，就需要加载 pyim-dregcache 包。
(require 'pyim-dregcache)
(setq pyim-dcache-backend 'pyim-dregcache)

;; 选词框显示5个候选词
(setq pyim-page-length 5)

(setq default-input-method "pyim")
(global-set-key (kbd "C-\\") 'toggle-input-method)

;; (require 'pyim-dict)
;; (require 'pyim-dict-manager)
(setq pyim-dicts
      '((:name "tsinghua" :file "~/.local/share/pyim/dict/pyim-tsinghua-dict.pyim")
        (:name "compute noun" :file "~/.local/share/pyim/dict/compute_noun.pyim")
        (:name "compute words" :file "~/.local/share/pyim/dict/compute_words.pyim")))

;; 让 Emacs 启动时自动加载 pyim 词库
(add-hook 'emacs-startup-hook
          #'(lambda () (pyim-restart-1 t)))

;; 金手指设置，可以将光标处的编码，比如：拼音字符串，转换为中文。
;; (global-set-key (kbd "M-j") 'pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
;; (global-set-key (kbd "C-;") 'pyim-delete-word-from-personal-buffer)


;; translation by google translate
(wen-require-package 'google-translate)
(require 'google-translate)
(require 'google-translate-default-ui)
(setq google-translate-default-source-language "auto")
(setq google-translate-default-target-language "zh")
(global-set-key (kbd "C-c M-\\") 'google-translate-at-point)

(provide 'core-chinese)

;;; core-chinese.el ends here
