;; custom.el

;; From: https://github.com/redguardtoo/emacs.d

;; custom-set was added by Custom.

;; --------------------------------------------------------
;; 设置有用的个人信息
(setq user-full-name "Jleafy")
(setq user-mail-address "jleafy@163.com")

;; BASIC CONFIG
;; --------------------------------------------------------
(menu-bar-mode 0)

;; 字体
(set-default-font "consolas-12")
(set-fontset-font "fontset-default"
    'gb18030' ("微软雅黑" . "unicode-bmp"))

;; windows平台Emacs单实例原理、设置及右键菜单的添加
;; make a dictionary in "~/.emacs.d/" named "server"
; "D:\Tools\emacs-25.1.2\bin\emacsclientw.exe" --no-wait --alternate-editor="D:\Tools\emacs-25.1.2\bin\runemacs.exe" "%1"
(server-start)

;; PATH and FILE - CONFIG
;; --------------------------------------------------------
(setq default-directory "~/Desktop/")
(setq command-line-default-directory "~/Desktop/")

(setq-default make-backup-files nil)
(setq make-backup-files nil)
;; 不生成 #filename# 临时文件
(setq auto-save-default nil)


(provide 'custom)
