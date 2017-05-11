;; init.el --- Emacs configuration

;; Author: Jleafy
;; Date: 2017-04-21
;; Version: 1.0
;; From: https://segmentfault.com/a/1190000004165173


;; ========================================================
;; INITIALIZATION
;; ========================================================

;; INSTALL PACKAGES
;; --------------------------------------------------------
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(require 'package)
(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    molokai-theme

    smex  ;; M-x complete
    tabbar  ;; tab pages and a tabbar ,for the windows style
    autopair
    ivy
    yasnippet  ;; yet another snippets
    auto-complete  ;; an popular auto complete system

    markdown-mode
    anaconda-mode
    flycheck  ;; add the flycheck package

    ; elpy  ;; add the elpy package(for python)
    ; ein  ;; add the ein package (Emacs ipython notebook)
    ; py-autopep8  ;; python pep8 format
    ))

(mapc #'(lambda (package)
  (unless (package-installed-p package)
    (package-install package)))
    myPackages)

;; my personal setup, other major-mode specific setup need it.
;; It's dependent on init-site-lisp.el
; (if (file-exists-p "~/.emacs.d/custom.el") (load-file "~/.emacs.d/custom.el"))
; (add-to-list 'load-path (expand-file-name "~/.emacs.d/custom.el"))

; (when (require 'time-date nil t)
;     (message "Emacs startup time: %d seconds."
;              (time-to-seconds (time-since emacs-load-start-time))))


;; ========================================================
;; BASIC CUSTOMIZATION
;; ========================================================

;; GUI - CONFIG
;; --------------------------------------------------------
(setq inhibit-startup-message t) ;; hide the startup message
; (setq gnus-inhibit-startup-message t)
; (setq visible-bell t)
(menu-bar-mode)
; (scroll-bar-mode 0)
; (tool-bar-mode 0)

;; 初始窗口大小
; (setq default-frame-alist
; '((height . 35) (width . 100) (menu-bar-lines . 20) (tool-bar-lines . 0)))

;; Theme
(load-theme 'molokai t)
;; load material theme
; (load-theme 'material t)

;; 字体
(set-default-font "consolas-12")
(set-fontset-font "fontset-default"
    'gb18030' ("微软雅黑" . "unicode-bmp"))

;; 界面显示行号
(global-linum-mode t) ; always show line numbers
(setq linum-format " %d ")  ;set format
;; 显示行列号,它显示在minibuffer上面那个杠上
(setq column-number-mode t)
(setq line-number-mode t)

;; 在标题栏提示当前位置
(setq frame-title-format "emacs@%b")
;; 允许minibuffer自由变化其大小（指宽度）
(setq resize-mini-windows t)

;; Show a marker in the left fringe for lines not in the buffer
; (setq indicate-empty-lines t)

;; windows平台Emacs单实例原理、设置及右键菜单的添加
;; make a dictionary in "~/.emacs.d/" named "sever"
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


;; EDITOR - CONFIG
;; --------------------------------------------------------
;; 打开就启用 text 模式
(setq default-major-mode 'text-mode)
(setq scroll-step 1 scroll-margin 3 scroll-conservatively 10000)

;; 语法高亮
(global-font-lock-mode t)
;; 高亮括号配对
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
; (electric-pair-mode)

;; 高亮显示选中的区域
(transient-mark-mode t)
;; highlight current line
(global-hl-line-mode t)

;; 美化显示符号（elisp），比如lambda会显示为λ
(prettify-symbols-mode)
(global-prettify-symbols-mode 1)

(setq kill-ring-max 200)

;; 光标显示为一竖线
(setq-default cursor-type 'bar)

;; 设置tab为4个空格的宽度
;;(setq default-tab-width 4)

(mouse-wheel-mode t)
;; 支持中键粘贴
(setq mouse-yank-at-point t)

;; 当指针移到另一行，不要新增这一行？d
(setq next-line-add-newlines nil)

;; 在行首 C-k 时，同时删除该行
(setq-default kill-whole-line t)

;; 允许emacs和外部其他程序的粘贴
(setq x-select-enable-clipboard t)
;; 让 Emacs 可以直接打开和显示图片。
(setq auto-image-file-mode t)
;; 当光标在行尾上下移动的时候，始终保持在行尾。
(setq track-eol t)
;; 鼠标自动避开指针
(mouse-avoidance-mode 'animate)


;; TIME and TIME-STAMP - CONFIG
;; --------------------------------------------------------
;; 启用时间显示设置，在minibuffer上面的那个杠上
(display-time-mode t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
; (setq display-time-interval 10)
; (setq display-time-use-mail-icon t)

;; 时间戳设置(time-stamp)，设定文档上次保存的信息
;; 只要里在你得文档里有Time-stamp:的设置，就会自动保存时间戳
;; 启用time-stamp
(setq time-stamp-active t)
;; 去掉time-stamp的警告？
(setq time-stamp-warn-inactive t)
;; 设置time-stamp的格式，我如下的格式所得的一个例子：
(setq time-stamp-format "%:u %02m/%02d/%04y %02H02M02S")
;; 将修改时间戳添加到保存文件的动作里。
(add-hook 'write-file-hooks 'time-stamp)


;; SOMETHING OTHERS - CONFIG
;; --------------------------------------------------------
;; 设置有用的个人信息
(setq user-full-name "Jleafy")
(setq user-mail-address "jleafy@163.com")

;; ido的配置,这个可以使你在用C-x C-f打开文件的时候在后面有提示;
;;这里是直接打开了ido的支持，在emacs23中这个是自带的.
(ido-mode t)
;; ido模式中不保存目录列表,解决退出Emacs时ido要询问编码的问题。
(setq ido-save-directory-list-file nil)

;; 当寻找一个同名的文件，自动关联上那个文件？
(setq uniquify-buffer-name-style 'forward)

;; 在emacs读man文档时，使用当前buffer
(setq Man-notify-method 'pushy)

;; 修改中文文本的行距,3个象素就可以了吧
; (setq-default line-spacing 3)

;; 启用C-x,C-v,C-s这些通用设置
; (cua-mode t)


; CODING - CONFIG
;; --------------------------------------------------------
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq default-process-coding-system 'utf-8)
; (setq-default pathname-coding-system 'utf-8)


;; BACKUP - CONFIG
;; --------------------------------------------------------
;;emacs还有一个自动保存功能，默认在~/.emacs.d/auto-save-list里
;; 启用版本控制，即可以备份多次
; (setq version-control t)
;; 备份最原始的版本两次，记第一次编辑前的文档，和第二次编辑前的文档
; (setq kept-old-versions 2)
;; 备份最新的版本五次，理解同上
; (setq kept-new-versions 5)
;; 删掉不属于以上7中版本的版本
; (setq delete-old-versions t)
;; 设置备份文件的路径
; (setq backup-directory-alist '(("." . "~/.emacs.tmp")))
;; 备份设置方法，直接拷贝
; (setq backup-by-copying t)


;; COLOR - CONFIG
;; --------------------------------------------------------
;; 指针颜色设置为白色
; (set-cursor-color "white")
;; 鼠标颜色设置为白色
; (set-mouse-color "white")
;; 设置背景颜色和字体颜色
; (set-foreground-color "white")
; (set-background-color "darkblue")
;; 设置另外一些颜色：语法高亮显示的背景和主题，区域选择的背景和主题，二次选择的背景和选择
; (set-face-foreground 'highlight "white")
; (set-face-background 'highlight "blue")
; (set-face-foreground 'region "cyan")
; (set-face-background 'region "blue")
; (set-face-foreground 'secondary-selection "skyblue")
; (set-face-background 'secondary-selection "darkblue")
;; 设置日历的一些颜色
; (setq calendar-load-hook
; '(lambda ()
; (set-face-foreground 'diary-face "skyblue")
; (set-face-background 'holiday-face "slate blue")
; (set-face-foreground 'holiday-face "white")))


;; ========================================================
;; PLUGINS CONFIGURATION
;; ========================================================

;; PLUGINS: smex
;; --------------------------------------------------------
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;; Change path for ~/.smex-items
(setq-default smex-save-file (expand-file-name ".smex-items" "~/.emacs.d/tmp/"))
; (global-set-key [remap execute-extended-command] 'smex)


;; PLUGINS: tabbar
;; --------------------------------------------------------
; (tabbar-mode)

; (global-set-key (kbd "<M-up>")    'tabbar-backward-group)
; (global-set-key (kbd "<M-down>")  'tabbar-forward-group)
; (global-set-key (kbd "<M-left>")  'tabbar-backward-tab)
; (global-set-key (kbd "<M-right>") 'tabbar-forward-tab)

; (setq
;  tabbar-scroll-left-help-function nil   ;don't show help information
;  tabbar-scroll-right-help-function nil
;  tabbar-help-on-tab-function nil
;  tabbar-home-help-function nil
;  tabbar-buffer-home-button (quote (("") "")) ;don't show tabbar button
;  tabbar-scroll-left-button (quote (("") ""))
;  tabbar-scroll-right-button (quote (("") "")))

; (defun my-tabbar-buffer-groups ()
;   "Return the list of group names the current buffer belongs to.  Return a list of one element based on major mode."
;   (list
;    (cond
;     ((or (get-buffer-process (current-buffer))
;          ;; Check if the major mode derives from `comint-mode' or
;          ;; `compilation-mode'.
;          (tabbar-buffer-mode-derived-p
;           major-mode '(comint-mode compilation-mode)))
;      "Process")
;     ((string-equal "*" (substring (buffer-name) 0 1)) "Emacs Buffer")
;     ((eq major-mode 'dired-mode) "Dired")
;     (t "User Buffer"))))

; (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)


;; PLUGINS: autopair
;; --------------------------------------------------------
(autopair-global-mode) ;; to enable in all buffers


;; PLUGINS: ivy
;; --------------------------------------------------------
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(setq-default ivy-use-virtual-buffers t
    ivy-count-format ""
    ivy-initial-inputs-alist
    '((counsel-M-x . "^")
      (man . "^")
      (woman . "^")))
;; IDO-style directory navigation
; (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
; (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
(add-hook 'after-init-hook
    (lambda ()
    (when (bound-and-true-p ido-ubiquitous-mode)
        (ido-ubiquitous-mode -1))
        (when (bound-and-true-p ido-mode)
            (ido-mode -1))
            (ivy-mode 1)))


;; PLUGINS: yasnippet
;; --------------------------------------------------------
(yas-global-mode 1)


;; PLUGINS: auto-complete
;; --------------------------------------------------------
(ac-config-default)
(setq ac-dwim t)  ;; 设置tab键的使用模式
(setq ac-use-quick-help nil)
; (setq ac-quick-help-delay 1.0)
(setq ac-auto-start 3) ;; 输入4个字符才开始补全
;; Show menu 0.8 second later
(setq ac-auto-show-menu 0.8)
;; menu设置为12 lines
(setq ac-menu-height 12)
;; 选择菜单项的快捷键
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
;; 将backspace的删除后仍旧可以触发ac补全
(setq ac-trigger-commands
      (cons 'backward-delete-char-untabify ac-trigger-commands))
(global-set-key "\M-/" 'auto-complete)  ;; 补全的快捷键，用于需要提前补全
(setq ac-comphist-file "~/.emacs.d/tmp/.ac-comphist.dat")
;set face
; (set-face-background 'ac-candidate-face "lightgray")
; (set-face-underline 'ac-candidate-face "darkgray")
; (set-face-background 'ac-selection-face "steelblue")


;; PLUGINS: popup
;; --------------------------------------------------------


; PLUGINS: markdown-mode
;; --------------------------------------------------------
; (use-package markdown-mode
;   :ensure t
;   :commands (markdown-mode gfm-mode)
;   :mode (("README\\.md\\'" . gfm-mode)
;          ("\\.md\\'" . markdown-mode)
;          ("\\.markdown\\'" . markdown-mode))
;   :init (setq markdown-command "multimarkdown"))


; PLUGINS: livedown
;; --------------------------------------------------------


; PLUGINS: anaconda-mode
;; --------------------------------------------------------
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)


;; PLUGINS: python (elpy, ein, autopep8)
;; --------------------------------------------------------
; (elpy-enable)
; (elpy-use-ipython)  ;; the ein package (Emacs ipython notebook)
; (setq ein:use-auto-complete t)

; ;; enable autopep8 formatting on save
; (require 'py-autopep8)
; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


;; PLUGINS: flycheck
;; --------------------------------------------------------
;; use flycheck not flymake with elpy
; (when (require 'flycheck nil t)
;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;   (add-hook 'elpy-mode-hook 'flycheck-mode))

(global-flycheck-mode)
; ; (add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook 'global-flycheck-mode)
(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)


; PLUGINS: flymake
;; --------------------------------------------------------
; (autoload 'flymake-find-file-hook "flymake" "" t)
; (add-hook 'find-file-hook 'flymake-find-file-hook)
; (setq flymake-gui-warnings-enabled nil)
; (setq flymake-log-level 0)


; PLUGINS: hippie-expand
;; --------------------------------------------------------
; (global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
    '(try-expand-line
    try-expand-line-all-buffers
    try-expand-list
    try-expand-list-all-buffers
    try-expand-dabbrev
    try-expand-dabbrev-visible
    try-expand-dabbrev-all-buffers
    try-expand-dabbrev-from-kill
    try-complete-file-name
    try-complete-file-name-partially
    try-complete-lisp-symbol
    try-complete-lisp-symbol-partially
    try-expand-whole-kill))

; (setq hippie-expand-try-functions-list
;       '(try-complete-file-name-partially
;         try-complete-file-name
;         try-expand-dabbrev
;         try-expand-dabbrev-all-buffers
;         try-expand-dabbrev-from-kill))


; PLUGINS: dired
;; --------------------------------------------------------


; PLUGINS: recentf
;; --------------------------------------------------------
(add-hook 'after-init-hook (lambda () (recentf-mode 1)))
(setq recentf-save-file "~/.emacs.d/tmp/.recentf")
(setq-default
    recentf-max-saved-items 1000
    recentf-exclude '("/tmp/" "/ssh:"))


; PLUGINS: powershell
;; --------------------------------------------------------
; (setq explicit-shell-file-name “c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe”)
; (setq explicit-powershell.exe-args ‘(“-Command” “-” )) ; interactive, but no command prompt
; (autoload 'powershell "powershell" "Run powershell as a shell within emacs." t)
; (put 'dired-find-alternate-file 'disabled nil)
; ;; add the dir to load path
; (add-to-list 'load-path "~/.emacs.d/lisp/")
; ;; autoload powershell interactive shell
; (autoload 'powershell "powershell" "Start a interactive shell of PowerShell." t)
; ;; powershell-mode
; (autoload 'powershell-mode "powershell-mode" "A editing mode for Microsoft PowerShell." t)
; (add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode)) ; PowerShell script


;; ========================================================
;; 快捷键绑定 CONFIGURATION
;; ========================================================

;; BASIC SHORTCUT - CONFIG
;; --------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)

; (global-set-key (kbd "C-j") 'goto-line)
; (global-set-key (kbd "C-/") 'undo)

;; 设置 M-空格 来 set-mark,不使用默认的 C-@
(global-set-key (kbd "M-<SPC>") 'set-mark-command)
; (global-set-key (kbd "M-/") 'set-mark-command)
; (global-set-key (kbd "C-\"") 'set-mark-command)

;; 移动光标设置到文件头/尾
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end]  'end-of-buffer)

;; 设置C->键作为窗口之间的切换，默认的是C-x-o,比较麻烦
; (global-set-key (kbd "C->") 'other-window)

;; 扩大或者缩小窗口（上下）,Ctrl+{}
(global-set-key (kbd "C-}") 'enlarge-window)
(global-set-key (kbd "C-{") 'shrink-window)

;; 设置 Ctrl-Enter 在当前行下方插入新行
; (global-set-key (kbd "C-<return>")
;     '(lambda ()
;     (interactive)
;     (move-end-of-line 1)
;     (newline)))


;; PROGRAMING SHORTCUT - CONFIG
;; --------------------------------------------------------
;; 按下Alt+/，就会弹出菜单让你自动补全
;;(define-key c-mode-base-map [(meta ?/)] 'semantic-ia-complete-symbol-menu)

; ;;热键设置
; (global-set-key [f5] 'complile)
;     (setq-default compile-command "make")
; (global-set-key [f6] 'speedbar)
; (global-set-key [f7] 'gdb)
; (global-set-key [f8] 'previous-error)
; (global-set-key [f9] 'next-error)
; ;;启动窗口gdb
; (global-set-key [f10] 'gdb-many-windows)

;;(global-set-key [f9] 'delete-window);F9 关闭d当前窗口
;;(global-set-key [f8] 'other-window);F8窗口间跳转
;;(global-set-key [(f2)] 'ansi-term);F2 切换到shell模式
;;(global-set-key [f3] 'split-window-vertically);F3分割窗口
;;(global-set-key [f12] 'my-fullscreen);F12 全屏
;;(global-set-key [(f4)] 'compile);编译
;;(global-set-key [f5] 'gdb);启动gdb
;;(global-set-key [(f6)] 'gdb-many-windows);启动窗口gdb
;;(global-set-key [f1] 'goto-line);设置M-g为goto-line
;;(global-set-key [f7] 'other-frame);跳到其它窗格
;;(global-set-key [(f3)] 'speedbar);打开speedbar


;; ========================================================
;; 自定义功能
;; ========================================================

;; 插入日期
;; --------------------------------------------------------
(defun insert-current-date ()
  "Insert the current date"
  (interactive "*")
;(insert (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))))
  (insert (format-time-string "%Y/%m/%d" (current-time))))
(global-set-key "\C-xd" 'insert-current-date)

;; 插入时间
;; --------------------------------------------------------
(defun insert-current-time ()
  "Insert the current time"
  (interactive "*")
;(insert (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))))
  (insert (format-time-string "%H:%M:%S" (current-time))))
(global-set-key "\C-xt" 'insert-current-time)

;; 拷贝代码自动格式化
;; --------------------------------------------------------
(dolist (command '(yank yank-pop))
 (eval
  `(defadvice ,command (after indent-region activate)
 (and (not current-prefix-arg)
  (member major-mode
   '(c-mode
   c++-mode
   clojure-mode
   emacs-lisp-mode
   haskell-mode
   js-mode
   latex-mode
   lisp-mode
   objc-mode
   perl-mode
   cperl-mode
   plain-tex-mode
   python-mode
   rspec-mode
   ruby-mode
   scheme-mode))
  (let ((mark-even-if-inactive transient-mark-mode))
  (indent-region (region-beginning) (region-end) nil))))))


;; ========================================================
;; init.el ends here
;; ========================================================

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
