;;; init.el --- Emacs configuration

;;; Author: Jleafy
;;; Date: 2017-08-13
;;; Version: 0.3.5
;;; From: http://home.thep.lu.se/~karlf/emacs.html
;;; Time-stamp: <Last changed 2017-08-013 14:34:12 by Jleafy, Jleafy>

;;; commentary:
;; Mimi version!

;;; Code:

;; ==================================================================
;; INITIALIZATION
;; ==================================================================

;; -*- emacs-lisp -*-
(message "Reading configuration file ...")

;; Initialize files
;; ------------------------------------------------------------------

(setq gc-cons-threshold 100000000)
; (setq gc-cons-threshold (* 100 1024 1024))

;; Where to find external lisp-files, for modes, etc.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(setq message-log-max t)  ;; keep message buffer complete.

;; garbage-collect on focus-out, Emacs should feel snappier.
(add-hook 'focus-out-hook #'garbage-collect)

;; Install Packages
;; ------------------------------------------------------------------

;; 1. package management
;; ------------------------------------------------------------------
(require 'package)
;; Since emacs 24 there's a very neat package system.
(when (>= emacs-major-version 24)
  (setq package-archives '(
    ;; mirrors in China
    ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
    ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
    ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")

    ;;("gnu" . "http://elpa.gnu.org/packages/")
    ;;("melpa" . "http://melpa.org/packages/")
    ;;("melpa-stable" . "http://stable.melpa.org/packages/")
    )))
(package-initialize)

;; 2. pinn package source
;; ------------------------------------------------------------------
;; If a package is available in multiple archives, we can set which ones to use for which package.
; (when (boundp 'package-pinned-packages)
;   (setq package-pinned-packages
;         '((smex               . "melpa-stable")
;           (zenburn-theme      . "melpa-stable") ; a nice theme
;           (anti-zenburn-theme . "melpa-stable") ; a nice theme (brighter)
;           (zen-and-art-theme  . "marmalade")    ; a nice theme (darker)
;           ;;(htmlize            . "marmalade")
;           ;;(rainbow-delimiters . "melpa-stable")
;           ;; "unstable" package
;           ;;(icicles            . "melpa")
;           )))

;; 3. package auto-install
;; ------------------------------------------------------------------
;; List of all the packages I have (want) installed.
(setq my-requireed-packages
  '(better-defaults
    monokai-theme
    material-theme           ; color theme
    molokai-theme            ; color theme
    solarized-theme

    smex                     ; smarter "M-x"
    autopair                 ; auto compleate brackets
    ivy                      ; similar to ido or helm
    yasnippet                ; auto-complete templates (e.g. if, for, do ...)
    flycheck                 ; check code sanity while I type
    auto-complete            ; auto-compleate
    ; company                  ; for auto-compleate variables/functions (with drop down menu)

    markdown-mode            ; markdown-mode for github posts
    pylint                   ; python check
    auctex                   ; for latex

    ;; tools
    sr-speedbar              ; hanced for speedbar
    which-key                ; show sub-keys after a second of an unfinished key-press
    indent-guide             ; show vertical lines to guide indentation
    highlight-symbol         ; highlight symbol
    ; powerline                ;
    ; undo-tree                ; treat undo history as a tree
    ; browse-kill-ring         ; interactively insert items from kill-ring
    ; volatile-highlights      ; minor mode for visual feedback on some operations
    ; bing-dict                ; brif En-Ch Bing dictionary

    ; htmlize                  ; convert buffer text and decorations to HTML
    ; benchmark-init           ; keep track of where time is being spent during Emacs startup

    ; matlab-mode              ; yuck! when you must, you must
    ; slime                    ; Superios Lisp Interaction Mode for Emacs

    ; python-mode              ; (optional)
    ; elpy                     ; add the elpy package (for python)
    ; ein                      ; add the ein package (emacs ipython notebook)
    ; py-autopep8              ; add the autopep8 package (for python)
    ; jedi                     ; python auto-completion for emacs (optional)
    ))

;; Install any packages in my-requireed-packages, if they are not installed already.
(let ((refreshed nil))
  (when (not package-archive-contents)
    (package-refresh-contents)
    (setq refreshed t))
  (dolist (pkg my-requireed-packages)
    (when (and (not (package-installed-p pkg))
               (assoc pkg package-archive-contents))
      (unless refreshed
        (package-refresh-contents)
        (setq refreshed t))
      (package-install pkg))))

;; Useful for cleaning out unwanted packages.
(defun package-list-unaccounted-packages ()
  "Show only the packages that are installed and are not in `my-required-packages'."
  (interactive)
  (package-show-package-list
    (cl-remove-if-not (lambda (x)
      (and (not (memq x my-requireed-packages))
            (not (package-built-in-p x))
                (package-installed-p x)))
      (mapcar 'car package-archive-contents))))


;; ==================================================================
;; BASIC CUSTOMIZATION
;; ==================================================================

;; ------------------------------------------------------------------
;; Path and User-Info - CONFIG
;; ------------------------------------------------------------------
(setq default-directory "~/Desktop/")
(setq command-line-default-directory "~/Desktop/")

;; 设置个人信息

;; windows平台Emacs单实例原理、设置及右键菜单的添加
;; "D:\Tools\emacs\bin\emacsclientw.exe" --no-wait --alternate-editor="D:\Tools\emacs\bin\runemacs.exe" "%1"
(server-start)

;; ------------------------------------------------------------------
;; USER BETTER DEFAULT - CONFIG
;; ------------------------------------------------------------------

;;; Initial window
(setq inhibit-startup-message t)   ;; hide the startup message
(setq inhibit-splash-screen t)     ;; cancel welcome page
(setq inhibit-startup-echo-area-message t)
(setq ring-bell-function 'ignore)  ;; forbid the ring bell

;;; Frame-layout (UI)
(if (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(if (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(setq use-file-dialog nil)
(setq use-dialog-box nil)
;; Show a marker in the left fringe for lines not in the buffer
; (setq indicate-empty-lines t)

;;; Theme
(load-theme 'monokai t)
; (load-theme 'solarized t)
; (load-theme 'molokai t)
; (load-theme 'material t)
; (load "theme/twilight-theme")

;;; Window Size
; (setq initial-frame-alist '((height . 42) (width . 145)))
; (setq default-frame-alist '((height . 40) (width . 143)))
;; open up with full screen
; (setq initial-frame-alist (quote ((fullscreen . maximized))))
; (add-hook 'window-setup-hook 'toggle-frame-maximized t)  ;; Start fullscreen

;;; Font
(set-frame-font "consolas-14")
(set-fontset-font "fontset-default"
    'gb18030' ("Microsoft YaHei" . "unicode-bmp"))
; (set-frame-font "-outline-Consolas-normal-normal-normal-mono-16-*-*-*-c-*-iso8859-1" nil t)
; (set-face-attribute 'default nil :height 120)  ;; set font size

;;; Line number
(global-linum-mode 'linum-mode)  ;; always show line numbers
; (setq linum-format " %d ")      ;; set format

;;; Minibuffer
(setq column-number-mode t)   ;; show row/colum number on minibuffer
(setq line-number-mode t)
(setq resize-mini-windows t)  ;; 允许minibuffer自由变化其大小（指宽度）
(file-name-shadow-mode t)     ;; be smart about file names in mini buffer

;;; Title
(setq frame-title-format "emacs@%b")  ;; 在标题栏提示当前文件名
; (setq frame-title-format '(buffer-file-name "%f" ("%b"))) ;; titlebar =buffer unless filename

;;; Cursor and Mouse
(setq-default cursor-type 'bar)   ;; modify the cursor
; (blink-cursor-mode 0)           ;; make cursor not blink
(mouse-avoidance-mode 'animate)   ;; mouse keep away from cursor
(mouse-wheel-mode t)
(setq mouse-yank-at-point t)      ;; surport middle-wheel-key past
(setq select-enable-clipboard t)  ;; 允许emacs和外部其他程序的粘贴
;; make {copy, cut, paste, undo} have {C-c, C-x, C-v, C-z} keys.
; (cua-mode 1)

;;; Major mode
(setq major-mode 'text-mode)
; (setq-default major-mode 'org-mode)  ;; set default mode for unknown files

;;; Scroll margin
(setq scroll-step 1 scroll-margin 3 scroll-conservatively 10000)
(setq kill-ring-max 200)

;;; Encoding UTF-8
; (set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq file-name-coding-system 'gbk)
(set-terminal-coding-system 'utf-8)

;;; Highlight current line
(when (fboundp 'global-hl-line-mode)
    (global-hl-line-mode t))   ;; turn it on for all modes by default
(transient-mark-mode t)        ;; 高亮显示选中的区域
(global-font-lock-mode t)      ;; 语法高亮

;;; Parantes-matchning
(when (fboundp 'show-paren-mode)
    (show-paren-mode t)       ;; turn on bracket match highlight
    ; (electric-pair-mode 1)  ;; auto insert closing bracket
    (setq show-paren-style 'parenthesis))

;;; Tab-width
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)  ;; tab改为插入空格
; (setq tab-width 4)  ;; for current buffer only

;;; Indent (RET)
;; make Return key also do indent, for current buffer only
; (electric-indent-local-mode 1)
(electric-indent-mode 1)  ;; globally

(delete-selection-mode 1)          ;; make typing delete/overwrites selected text

(setq auto-image-file-mode t)      ;; enable to open image

(global-auto-revert-mode 1)        ;; refresh file automatically

(global-subword-mode 1)            ;; make cursor movement stop in between camelCase words

(global-prettify-symbols-mode 1)   ;; 美化显示符号（elisp），比如lambda会显示为λ

(setq next-line-add-newlines nil)  ;; 当指针移到另一行，不要新增这一行
(setq-default kill-whole-line t)   ;; 在行首 C-k 时，同时删除该行
(setq track-eol t)                 ;; 当光标在行尾上下移动的时候，始终保持在行尾
; (setq-default require-final-newline t)  ;; Make sure there is a final newline

;;; Initial scratch Message
; (setq initial-scratch-message ";; scratch buffer created -- happy hacking.\n")
(setq initial-scratch-message
  (concat ";; scratch buffer created, welcome to "
    (substring (emacs-version) 0 16) ".\n"))

; (setq pop-up-frames t)          ;; each file opens in a new window
; (setq-default line-spacing 3)   ;; 修改中文文本的行距, 3个像素


;; ------------------------------------------------------------------
;; BACKUP and AUTO-SAVE - CONFIG
;; ------------------------------------------------------------------

;; backup path settings
(defvar backup-directory (concat user-emacs-directory "var/emacs-backup"))
(if (not (file-exists-p backup-directory))
    (make-directory backup-directory t))

;; Sets all files to be backed up and auto saved in a single directory.
(setq backup-directory-alist `((".*" . ,backup-directory))
      auto-save-file-name-transforms `((".*" ,backup-directory t)))

(setq make-backup-files t        ;; backup of a file the first time it is saved.
    backup-by-copying t          ;; don't clobber symlinks
    version-control t            ;; version numbers for backup files
    kept-new-versions 2
    kept-old-versions 5
    delete-old-versions t        ;; delete excess backup files silently
    delete-by-moving-to-trash t
    make-backup-files nil        ;; No annoying "~file.txt"
    auto-save-default nil)       ;; no auto saves to #file#

;; ------------------------------------------------------------------
;; TIME and TIME-STAMP - CONFIG
;; ------------------------------------------------------------------

;;; Show Time on modeline
(display-time-mode t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

;;; Time-stamp
;; when there is a "Time-stamp: <>" in the first 10 lines of the file,
;; emacs will write time-stamp information there when saving the file.
(setq time-stamp-active t          ;; do enable time-stamps
      time-stamp-line-limit 10     ;; check first 10 buffer lines for Time-stamp: <>
      ; time-stamp-warn-inactive t
      ; time-stamp-format "%:u %02m/%02d/%04y %02H02M02S"
      time-stamp-format "Last changed %04y-%02m-%02d %02H:%02M:%02S by %L, %u")  ;; date format
(add-hook 'write-file-hooks 'time-stamp)  ;; update when saving

;; ------------------------------------------------------------------
;; SOMETHING OTHERS - CONFIG
;; ------------------------------------------------------------------

;; 当寻找一个同名的文件，自动关联上那个文件
(setq uniquify-buffer-name-style 'forward)

;; 在emacs读man文档时，使用当前buffer
(setq Man-notify-method 'pushy)

;; Enable disabled command: turn on upcase/downcase region
(put 'upcase-region 'disabled nil)    ;; same as M-u but on whole regions C-x C-u
(put 'downcase-region 'disabled nil)  ;; same as M-l but on whole regions C-x C-l

;; Whitespace cleanup before buffers are saved
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Make URLs in comments/strings clickable
(add-hook 'find-file-hooks 'goto-address-prog-mode)

;; Make shell scrips executable on save. Good!
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; When compiling from shell, display error result as in compilation buffer, with links to errors.
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

;; Instead of C-u C-SPC C-u C-SPC to pop mark twice, do: C-u C-SPC C-SPC
(setq set-mark-command-repeat-pop t)

;; Bookmarks
;; ------------------------------------------------------------------
;; C-x r m ('make'): create a new bookmark,
;; C-x r b ('bookmark'): jump to an existing bookmark,
;; C-x r l ('list'): see the list of your bookmarks.
(setq bookmark-default-file "~/.emacs.d/var/.bookmarks"
      bookmark-save-flag 1)  ;; auto save changes

;; Save History
;; ------------------------------------------------------------------
;; Save mode-line history between sessions. Very good!
(setq savehist-additional-variables    ;; Also save ...
  '(search-ring regexp-search-ring)    ;; ... searches
  savehist-file "~/.emacs.d/var/.savehist") ;; keep home clean
(savehist-mode t)                      ;; do this before evaluation

;; Awesome copy/paste
;; ------------------------------------------------------------------
;; If nothing is marked/highlighted, and you copy or cut
;; (C-w or M-w) then use column 1 to end. No need to "C-a C-k" or "C-a C-w" etc.

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

;; Register
;; ------------------------------------------------------------------
;; It's what prefixed by "C-x r". Allows storing of rectangular selections, copy, paste,
;; multiple cut/paste, etc, and remember positions in a buffer (for that session).
;; Set register (bookmark) with C-x r SPC [number], and jump to it with C-x r j [number].

;; Recenter after jump-to-register, when jumping, put mark on top of screen, not bottom, as is default
(defadvice jump-to-register
  (after jump-to-register-recenter-top)
  "Recenter point to top of window after jumping to a register."
  (recenter 0))
(ad-activate 'jump-to-register)

;; Useful to display list of the registers
(when (require 'list-register nil 'noerror)
  (global-set-key (kbd "C-x r v") 'list-register))

;; Uniquify buffers
;; ------------------------------------------------------------------
;; When several buffers have the same name, make the name uniqe by including part of path in name.
(when (require 'uniquify nil 'noerror)  ;; make buffer names more unique
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator ":"
        uniquify-after-kill-buffer-p t       ;; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*"))  ;; don't muck with special buffers

;; abbrevs
;; ------------------------------------------------------------------
; (setq save-abbrevs t)                 ;; (ask) save abbrevs when files are saved
; (setq-default abbrev-mode t)          ;; turn it on for all modes
(setq abbrev-file-name "~/.emacs.d/var/abbrev_defs.el")

(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))           ;;  don't tell
; (add-hook 'kill-emacs-hook 'write-abbrev-file)  ;; write when exiting emacs


;; ==================================================================
;; PLUGINS CONFIGURATION
;; ==================================================================

;; PLUGINS: smex
;; ------------------------------------------------------------------
;; smex - a smarter M-x compleation
(with-eval-after-load "smex"
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (setq smex-save-file "~/.emacs.d/var/.smex-items"))
; (global-set-key [remap execute-extended-command] 'smex)

;; PLUGINS: autopair
;; ------------------------------------------------------------------
; (autoload 'autopair-global-mode "autopair" nil t)
(when (require 'autopair nil 'noerror)
  ;; Use paredit intead in any lsip-environment:
  (add-hook 'emacs-lisp-mode-hook       '(lambda () (setq autopair-dont-activate t)))
  (add-hook 'lisp-mode-hook             '(lambda () (setq autopair-dont-activate t)))
  (add-hook 'lisp-interaction-mode-hook '(lambda () (setq autopair-dont-activate t)))
  (add-hook 'scheme-mode-hook           '(lambda () (setq autopair-dont-activate t)))
  (autopair-global-mode)  ;; to enable in all buffers
  (setq autopair-blink 'nil))

;; PLUGINS: ivy
;; ------------------------------------------------------------------
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
;; ------------------------------------------------------------------
; (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
; (add-hook 'prog-mode-hook #'yas-minor-mode)
(yas-global-mode 1)

;; PLUGINS: flycheck
;; ------------------------------------------------------------------
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

;; PLUGINS: auto-complete
;; ------------------------------------------------------------------
(ac-config-default)
(setq ac-dwim t)                     ;; 设置tab键的使用模式
(setq ac-use-quick-help nil
      ; setq ac-quick-help-delay 1.0
      ac-auto-start 2                ;; 输入3个字符才开始补全
      ac-auto-show-menu 0.5          ;; Show menu 0.8 second later
      ac-menu-height 12              ;; menu设置为12 lines
      ac-use-menu-map t)             ;; 选择菜单项的快捷键
(setq ac-comphist-file "~/.emacs.d/var/.ac-comphist.dat")

;; 将backspace的删除后仍旧可以触发ac补全
(setq ac-trigger-commands
    (cons 'backward-delete-char-untabify ac-trigger-commands))

(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
; (global-set-key "\M-/" 'auto-complete)  ;; 补全的快捷键，用于需要提前补全
(global-set-key (kbd "<C-tab>") 'auto-complete)  ;; 补全快捷键

;; set face
; (set-face-background 'ac-candidate-face "lightgray")
; (set-face-underline 'ac-candidate-face "darkgray")
; (set-face-background 'ac-selection-face "steelblue")

;; PLUGINS: company
;; ------------------------------------------------------------------
; (global-company-mode t)
; (setq company-idle-delay nil
;       company-minimum-prefix-length 2
;       company-require-match nil
;       company-dabbrev-ignore-case nil
;       company-dabbrev-downcase nil
;       company-show-numbers t
;       company-transformers '(company-sort-by-backend-importance)
;       company-continue-commands '(not helm-dabbrev)
;       )
; ; company backends to use anaconda for python
; ; (add-to-list 'company-backends '(company-anaconda :with company-yasnippet))

; ; 补全菜单选项快捷键
; (define-key company-active-map (kbd "C-n") 'company-select-next)
; (define-key company-active-map (kbd "C-p") 'company-select-previous)
; (global-set-key (kbd "<C-tab>") 'company-complete)  ;; 补全快捷键

;; PLUGINS: popup
;; ------------------------------------------------------------------

;; PLUGINS: markdown-mode
;; ------------------------------------------------------------------

;; PLUGINS: matlab-mode
;; ------------------------------------------------------------------
; (autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
; (autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
; (setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))

;; PLUGINS: anaconda-mode
;; ------------------------------------------------------------------
; (add-hook 'python-mode-hook 'anaconda-mode)
; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)

; ;; PLUGINS: python IDE (elpy, ein, autopep8, pylint)
; ;; ------------------------------------------------------------------
; ;; install the required Python packages: pip install flake8 importmagic autopep8
; (elpy-enable)
; (elpy-use-ipython)

; (when (require 'flycheck nil t)
;     (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;     (add-hook 'elpy-mode-hook 'flycheck-mode))

; ; (require 'py-autopep8)
; ; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
; ; (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)  ;; jedi config
; ; (setq ein:use-smartrep t)  ;; smartrep config

(add-hook 'python-mode-hook 'pylint-add-menu-items)
(add-hook 'python-mode-hook 'pylint-add-key-bindings)
; ; (defun flymake-pylint-init ()
; ;   (list python-python-command
; ;         (list "-m" "pylint.lint" "-f" "parseable" buffer-file-name)))
; ; (add-to-list 'flymake-allowed-file-name-masks
; ;               '("\\.py\\'" flymake-pylint-init))

;; PLUGINS: ido
;; ------------------------------------------------------------------
(ido-mode t)
;; ido模式中不保存目录列表,解决退出Emacs时ido要询问编码的问题。
(setq ido-save-directory-list-file nil)
; Disable the merging (the "looking in other directories" in ido vulgo), or switch with 'C-z' manually
(setq ido-auto-merge-work-directories-length -1)
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-use-virtual-buffers t)

(defun bind-ido-keys ()
  "Keybindings for ido mode."
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'bind-ido-keys)

;; PLUGINS: org-mode
;; ------------------------------------------------------------------
(setq org-src-fontify-natively t)  ;; org-mode 代码高亮

;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)

;; 自动换行
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(setq org-log-done t)
;; replace the "..." with "…" for collapsed org-mode content
(setq org-ellipsis "…")
;; RET follows hyperlinks in org-mode:
(setq org-return-follows-link t)

;; Use abbrev-minor-mode with org-mode: (I have global abbrev mode now)
; (add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

;; PLUGINS: recentf
;; ------------------------------------------------------------------
(add-hook 'after-init-hook (lambda () (recentf-mode 1)))
(setq recentf-save-file "~/.emacs.d/var/.recentf")
(setq-default recentf-max-saved-items 100
              recentf-exclude '("/var/" "/ssh:"))

(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; PLUGINS: hippie-expand
;; ------------------------------------------------------------------
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
; (setq dabbrev-case-replace nil)  ;; preserve case on expand with dabbrev

;; PLUGINS: flymake
;; ------------------------------------------------------------------
; (autoload 'flymake-find-file-hook "flymake" "" t)
; (add-hook 'find-file-hook 'flymake-find-file-hook)
; (setq flymake-gui-warnings-enabled nil)
; (setq flymake-log-level 0)

;; PLUGINS: saveplace
;; ------------------------------------------------------------------
;; When you visit a file, point goes to the last place where it was when you previously visited the same file.
;; Save point position between sessions.
; (require 'saveplace)
; (setq-default save-place t)
; (setq save-place-file (expand-file-name "var/.saved-places" user-emacs-directory))
; ; (setq save-place-file (concat user-emacs-directory "var/saved-places"))

;; PLUGINS: dired
;; ------------------------------------------------------------------
;; 延迟 dired load
(with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;; set recursive deletes and copies
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

(put 'dired-find-alternate-file 'disabled nil)  ;; set only one buffer for dired mode

;; allows you to use keybindings: C-x C-j to enter the path of current file folder
(require 'dired-x)

;; allows you to copy the contents to the other window when more than two windows are available in a frame.
(setq dired-dwin-target t)


;; PLUGINS: sr-speedbar
;; ------------------------------------------------------------------
; (add-hook 'after-init-hook '(lambda () (sr-speedbar-toggle)))  ;; 开启程序即启用
(setq sr-speedbar-right-side nil)
(setq speedbar-show-unknown-files t)
; (setq sr-speedbar-width 30)
(setq speedbar-use-images nil)

(global-set-key (kbd "<f6>")
  (lambda()
    (interactive)
    ; (set-frame-width (selected-frame) 120)
    (sr-speedbar-toggle)))
; (global-set-key (kbd "<C-f6>") 'speedbar-get-focus)

;; PLUGINS: which-key
;; ------------------------------------------------------------------
;; After 1 second of an unfinished key-press, show the documentation of the
;; sub-keys available in the key sequence.
(when (require 'which-key nil 'noerror)
  (which-key-mode))

;; PLUGINS: indent-guide
;; ------------------------------------------------------------------
; (require 'indent-guide)
(indent-guide-global-mode)
;; to show not only one guide line but all guide lines recursively.
(setq indent-guide-recursive t)

;; PLUGINS: highlight-symbol
;; ------------------------------------------------------------------
; (require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; PLUGINS: powerline
;; ------------------------------------------------------------------
; (powerline-default-theme)

; ;; PLUGINS: undo-tree
; ;; ------------------------------------------------------------------
; (when (require 'undo-tree nil 'noerror)
;     (global-undo-tree-mode))
; ;; 使用方法：C-x u 进入 undo-tree-visualizer-mode; p n 上下移动;
; ;; b f 在分支左右切换; t 显示时间戳，选定需要的状态后，q 退出。

; ;; PLUGINS: browse-kill-ring
; ;; ------------------------------------------------------------------
; ;; this makes M-y activate the kill-ring IF the previous command
; ;; was not a yank. (C-y (or C-v in my case))
; (when (require 'browse-kill-ring nil 'noerror)
;   (browse-kill-ring-default-keybindings)
;   (setq browse-kill-ring-quit-action 'save-and-restore))

; ;; PLUGINS: volatile-highlights
; ;; ------------------------------------------------------------------
; ;; Highlight the latest changes in the buffer (like text inserted from: yank, undo, etc.)
; ;; until the next command is run. Nice, since it lets me see exactly what was changed.
; (when (require 'volatile-highlights nil 'noerror)
;   (volatile-highlights-mode t))

; ;; PLUGINS: bing-dict
; ;; ------------------------------------------------------------------
; (setq bing-dict-pronunciation-style 'uk)
; ; (browse-url    ;; using the external browser
; ;  (concat "http://www.bing.com/dict/search?mkt=zh-cn&q="
; ;        (url-hexify-string
; ;         (read-string "Query: "))))
; (global-set-key (kbd "C-c d") 'bing-dict-brief)

;; PLUGINS: htmlize
;; ------------------------------------------------------------------
;; convert buffer (with type face, and syntax highlighting) to *.html
; (autoload 'htmlize-region "htmlize" "htmlize the region" t)
; (autoload 'htmlize-buffer "htmlize" "htmlize the buffer" t)

;; PLUGINS: benchmark-init
;; ------------------------------------------------------------------
; (benchmark-init/activate)

;; PLUGINS: tabbar
;; ------------------------------------------------------------------
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

;; PLUGINS: powershell
;; ------------------------------------------------------------------
; (setq explicit-shell-file-name “c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe”)
; (setq explicit-powershell.exe-args ‘(“-Command” “-” ))  ;; interactive, but no command prompt
; (autoload 'powershell "powershell" "Run powershell as a shell within emacs." t)
; (put 'dired-find-alternate-file 'disabled nil)
; ;; add the dir to load path
; (add-to-list 'load-path "~/.emacs.d/lisp/")
; ;; autoload powershell interactive shell
; (autoload 'powershell "powershell" "Start a interactive shell of PowerShell." t)
; ;; powershell-mode
; (autoload 'powershell-mode "powershell-mode" "A editing mode for Microsoft PowerShell." t)
; (add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode)) ; PowerShell script


;; ==================================================================
;; SPECIAL MODE
;; ==================================================================

;; ------------------------------------------------------------------
;; * C++ Mode *
;; ------------------------------------------------------------------

;; 1. Generic
;; ------------------------------------------------------------------
;; C++-specific. Which extensions should be associated with C++ (rather than C)
(add-to-list 'auto-mode-alist '("\\.h$"  . c++-mode)) ;h-files
(add-to-list 'auto-mode-alist '("\\.icc" . c++-mode)) ;implementation files
(add-to-list 'auto-mode-alist '("\\.tcc" . c++-mode)) ;files with templates

;; Indentation style:
;; (add-hook 'c-mode-common-hook '(lambda () (c-set-style "stroustrup")))
;; (add-hook 'c-mode-common-hook '(lambda () (c-set-style "linux")))

;; 2. Better compile buffer
;; ------------------------------------------------------------------
(require 'compile)
(add-hook 'c-mode-common-hook
  (lambda ()
    (setq
     compilation-scroll-output 'first-error   ; scroll until first error
     ;; compilation-read-command nil          ; don't need enter
     compilation-window-height 11)

    (local-set-key (kbd "<M-up>")   'previous-error)
    (local-set-key (kbd "<M-down>") 'next-error)

    (unless (file-exists-p "Makefile")
      (set (make-local-variable 'compile-command)
           ;; emulate make's .c.o implicit pattern rule, but with
           ;; different defaults for the CC, CPPFLAGS, and CFLAGS
           ;; variables:
           ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
           (let ((file (file-name-nondirectory buffer-file-name)))
             (format "%s %s -o %s %s"
                 (or (getenv "CC") "g++")
                 file
                 (file-name-sans-extension file)
                 ;;(or (getenv "CPPFLAGS") "-DDEBUG=9")
                 (or (getenv "CFLAGS") " -g -O2 -Wall -std=c++11")
                 )))))
  ;; (number of things in " " in format must match number of arg. in getenv.)

  ;; This will run Make if there is a Makefile in the same directory as the
  ;; source-file, or it will create a command for compiling a single
  ;; file and name the executable the same name as the file with the extension stripped.
  )

;; 3. Show function name in mod-line
;; ------------------------------------------------------------------
(add-hook 'c-mode-common-hook
  (lambda ()
    (which-function-mode t)))

;; 4. Navigate .h och .cpp
;; ------------------------------------------------------------------
;; Now, we can quickly switch between myfile.cc and myfile.h with C-c o.
;; Note the use of the c-mode-common-hook, so it will work for both C and C++.
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;; ------------------------------------------------------------------
;; * Text Mode *
;; ------------------------------------------------------------------

;; Line breaks / long lines
(defvar soft-line-breaks-p nil   ; nil or t
  "Use hard or soft line breaks for long lines.")

;; M-q doesn't insert double space after period.
(setq sentence-end-double-space nil)

(if soft-line-breaks-p
    (add-hook 'text-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'text-mode-hook
            '(lambda ()
               (set-fill-column 78)       ; lines are 78 chars long ...
               (auto-fill-mode t))))      ; ...and wrapped around automagically

;; ------------------------------------------------------------------
;; * GUD Mode * (gdb debugging)
;; ------------------------------------------------------------------

;; Make up/down behave as in terminal run it like this "M-x gdb",
;  gdb --annotate=3 ./yourBinary
(add-hook 'gud-mode-hook
  '(lambda ()
     (local-set-key [home] ; move to beginning of line, after prompt
                    'comint-bol)
     (local-set-key [up]   ; cycle backward through command history
                    '(lambda () (interactive)
                       (if (comint-after-pmark-p)
                           (comint-previous-input 1)
                         (previous-line 1))))
     (local-set-key [down] ; cycle forward through command history
                    '(lambda () (interactive)
                       (if (comint-after-pmark-p)
                           (comint-next-input 1)
                         (forward-line 1))))
    )
  (setq gdb-many-windows t))

;; ------------------------------------------------------------------
;; * Winner Mode *
;; ------------------------------------------------------------------
;; Useful to switch between different/previous buffer-layouts, or buffer history
;; cycle through window layouts/contents
;; winner conflicts with org, use C-c left/right instead
(when (require 'winner nil 'noerror)
  (setq winner-dont-bind-my-keys t)
  (global-set-key (kbd "<C-c left>")  'winner-undo)
  (global-set-key (kbd "<C-c right>") 'winner-redo)
  (winner-mode t))

;; ------------------------------------------------------------------
;; * ediff Mode *
;; ------------------------------------------------------------------
;; Don't use strange separate control-window.
(customize-set-variable 'ediff-window-setup-function 'ediff-setup-windows-plain)

;; Side by side comparison is easier than vertical split (tob-bottom-stacked) window
(customize-set-variable 'ediff-split-window-function 'split-window-horizontally)

;; reset the window configuration after ediff is done (winner-mode)
;;(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; ------------------------------------------------------------------
;; * Programming Mode *
;; ------------------------------------------------------------------
;; Check syntax while typing code:
; (add-hook 'prog-mode-hook #'flycheck-mode)

(setq compilation-ask-about-save nil)  ;; don't ask me to save _all_ buffers
(setq compilation-scroll-output 'first-error)  ;; Stop on the first error
(setq compilation-skip-threshold 2)  ;; Don't stop on info or warnings

;; ------------------------------------------------------------------
;; * LaTeX Mode * (auctex)
;; ------------------------------------------------------------------
;; 1. Miscellaneous
;; choose one, for what happens with long lines:
; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
; (add-hook 'LaTeX-mode-hook 'auto-fill-mode)

(add-hook 'LaTeX-mode-hook
    '(lambda ()
        (ispell-change-dictionary "american" nil)
        ;; Make equations into images & show in emacs:
        (autoload 'latex-math-preview-expression "latex-math-preview" nil t)
        (autoload 'latex-math-preview-save-image-file "latex-math-preview" nil t)
        (autoload 'latex-math-preview-beamer-frame "latex-math-preview" nil t)))

(setq font-latex-fontify-script nil ;; Don't fontify sub/super: _ ^
      TeX-auto-save t               ;; enable parse on save
      TeX-parse-self t              ;; enable parse on load
      TeX-auto-untabify t           ;; remove Tabs at save
      ispell-check-comments nil)    ;; don't spell check comments

;; Auto choose Swedish if usepackage{babel}[swedish]
;; so that:  Shift+2-> '' rather than ", or similar...
(add-hook 'TeX-language-sv-hook
    (lambda() (ispell-change-dictionary "svenska")))

;; 2. Math-$$-matchning
(setq LaTeX-mode-hook'
    (lambda () (defun TeX-insert-dollar ()
            "custom redefined insert-dollar"
            (interactive)
            (insert "$$")           ;; in LaTeX mode, typing "$" automatically
            (backward-char 1))))    ;; insert "$$" and move back one char.

;; 3. RefTeX awesomeness
;; Navigate sections by right mouse button. Similar to as C-c =
(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)            ;; with AUCTeX LaTeX mode

(autoload 'reftex-mode    "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" t)

;; To integrate RefTex even closer with AUCTeX.  E.g: when C-c C-s
;; or C-c C-e is called, AUCTex will call RefTeX, which will insert
;; a label automatically instead of having AUCTeX ask you for one;
;; When C-c C-s AUCTeX will update section list in RefTeX; RefTeX
;; will also tell AUCTeX about new label, citation, and index keys,
;; and add them to completions list.
(setq reftex-plug-into-AUCTeX t)

;; Make C-u prefixed commands not re-parse entire doc.
(setq reftex-enable-partial-scans t)

;; Even with partial-scan enables, reftex must make one full scan,
;; this saves the result to a file "*.rel"
; (setq reftex-save-parse-info t)

;; use separate buffer for selecting each label type
; (setq reftex-use-multiple-selection-buffers t)
;; ------------------------------------------------------------------


;; ==================================================================
;; 自定义功能(函数)
;; ==================================================================

;; 快速打开配置文件
;; ------------------------------------------------------------------
(defun open-init-file()
  "To open the init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<C-f12>") 'open-init-file)

(defun open-home-path()
  "To open the home path."
  (interactive)
  (find-file "~/"))
(global-set-key (kbd "<f12>") 'open-home-path)

(defun copy-buffer-file-path ()
  "Put current buffer's short path into the kill ring."
  (interactive)
  (when (buffer-file-name)
    (kill-new (f-short (buffer-file-name)))))

(defun copy-buffer-file-name ()
  "Put current buffer's base name into the kill ring."
  (interactive)
  (when (buffer-file-name)
    (kill-new (f-filename (buffer-file-name)))))
;; ------------------------------------------------------------------

;; 插入日期和时间
;; ------------------------------------------------------------------
;; 插入日期
(defun insert-current-date ()
  "Insert the current date."
  (interactive "*")
  ; (insert (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))))
  (insert (format-time-string "%Y/%m/%d" (current-time))))
(global-set-key "\C-xd" 'insert-current-date)

;; 插入时间
(defun insert-current-time ()
  "Insert the current time."
  (interactive "*")
  ; (insert (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))))
  (insert (format-time-string "%H:%M:%S" (current-time))))
(global-set-key "\C-xt" 'insert-current-time)
;; ------------------------------------------------------------------

;; Move line Up and Down
;; ------------------------------------------------------------------
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
    (exchange-point-and-mark))
    (let ((column (current-column))
      (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
    (forward-line)
    (when (or (< arg 0) (not (eobp)))
      (transpose-lines arg))
    (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line ARG lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line ARG lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

;; 使用 Ctrl + Alt + 方向健，上下移动行或者选定区域
(global-set-key [C-M-up] 'move-text-up)
(global-set-key [C-M-down] 'move-text-down)
;; ------------------------------------------------------------------

;; Linum-config
;; ------------------------------------------------------------------
;; 1.Linum: fix margins when font is scaled
(defun linum-update-window-scale-fix (win)
  "fix linum for scaled text"
  (set-window-margins win
      (ceiling (* (if (boundp 'text-scale-mode-step)
    (expt text-scale-mode-step
        text-scale-mode-amount) 1)
      (if (car (window-margins))
          (car (window-margins)) 1)
      ))))
(advice-add #'linum-update-window :after #'linum-update-window-scale-fix)

;; 2.Linum: Select lines by clicking
(defvar *linum-mdown-line* nil)

(defun line-at-click ()
  (save-excursion
    (let ((click-y (cdr (cdr (mouse-position))))
      (line-move-visual-store line-move-visual))
      (setq line-move-visual t)
      (goto-char (window-start))
      (next-line (1- click-y))
      (setq line-move-visual line-move-visual-store)
      ;; If you are using tabbar substitute the next line with
      ;; (line-number-at-pos))))
      (1+ (line-number-at-pos)))))

(defun md-select-linum ()
  (interactive)
  (goto-line (line-at-click))
  (set-mark (point))
  (setq *linum-mdown-line*
    (line-number-at-pos)))

(defun mu-select-linum ()
  (interactive)
  (when *linum-mdown-line*
    (let (mu-line)
      ;; (goto-line (line-at-click))
      (setq mu-line (line-at-click))
      (goto-line (max *linum-mdown-line* mu-line))
      (set-mark (line-end-position))
      (goto-line (min *linum-mdown-line* mu-line))
      (setq *linum-mdown*
    nil))))

(global-set-key (kbd "<left-margin> <down-mouse-1>") 'md-select-linum)
(global-set-key (kbd "<left-margin> <mouse-1>") 'mu-select-linum)
(global-set-key (kbd "<left-margin> <drag-mouse-1>") 'mu-select-linum)

;; 3.Linum: highlight the current line number
(require 'linum)
(defvar linum-current-line 1 "Current line number.")
(defvar linum-border-width 1 "Border width for linum.")

(defface linum-current-line
  `((t :inherit linum
       :foreground "goldenrod"
       :weight bold
       ))
  "Face for displaying the current line number."
  :group 'linum)

(defadvice linum-update (before advice-linum-update activate)
  "Set the current line."
  (setq linum-current-line (line-number-at-pos)
        ;; It's the same algorithm that linum dynamic. I only had added one
        ;; space in front of the first digit.
        linum-border-width (number-to-string
                            (+ 1 (length
                                  (number-to-string
                                   (count-lines (point-min) (point-max))))))))

(defun linum-highlight-current-line (line-number)
  "Highlight the current LINE-NUMBER using 'linum-current-line' face."
  (let ((face (if (= line-number linum-current-line)
                  'linum-current-line
                'linum)))
    (propertize (format (concat "%" linum-border-width "d") line-number)
                'face face)))

(setq linum-format 'linum-highlight-current-line)

;; 4.Linum: Separating line numbers from text
;; ------------------------------------------------------------------

;; Revert buffers
;; ------------------------------------------------------------------
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files."))
;; ------------------------------------------------------------------

;; Search all open buffers
;; ------------------------------------------------------------------
(defun search (regexp)
  "Search all buffers for a REGEXP."
  (interactive "sRegexp to search for: ")
  (multi-occur-in-matching-buffers ".*" regexp))
; (global-set-key (kbd "C-c o") 'multi-occur-in-matching-buffers)
;; ------------------------------------------------------------------

;; Nuke-all-buffers
;; ------------------------------------------------------------------
(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapc (lambda (x) (kill-buffer x)) (buffer-list)) (delete-other-windows))
;; ------------------------------------------------------------------

;; Kill all other buffers
;; ------------------------------------------------------------------
;; kills all buffers, except the current one.
(defun kill-all-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
;; ------------------------------------------------------------------

;; word lookup
;; ------------------------------------------------------------------
(defun lookup-word-definition ()
  "Look up the current word's definition in a browser. If a region is active (a phrase), lookup that phrase."
  (interactive)
  (let (myWord myUrl)
    (setq myWord
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'symbol)))

    (setq myWord (replace-regexp-in-string " " "%20" myWord))
    ;; url: http://www.bing.com/dict/search?mkt=zh-cn&q=
    (setq myUrl (concat "http://www.bing.com/dict/search?mkt=zh-cn&q=" myWord))

    (browse-url myUrl)
    ;; (w3m-browse-url myUrl)  ;; if you want to browse using w3m
    ))
(global-set-key (kbd "<C-f1>") 'lookup-word-definition)
;; ------------------------------------------------------------------

;; popup-shell
;; ------------------------------------------------------------------
(defvar th-shell-popup-buffer nil)

(defun th-shell-popup ()
  "Toggle a shell popup buffer with the current file's directory as cwd."
  (interactive)
  (unless (buffer-live-p th-shell-popup-buffer)
    (save-window-excursion (shell "*Popup Shell*"))
    (setq th-shell-popup-buffer (get-buffer "*Popup Shell*")))
  (let ((win (get-buffer-window th-shell-popup-buffer))
        (dir (file-name-directory (or (buffer-file-name)
                                      ;; dired
                                      dired-directory
                                      ;; use HOME
                                      "~/"))))
    (if win
        (quit-window nil win)
      (pop-to-buffer th-shell-popup-buffer nil t)
      (comint-send-string nil (concat "cd " dir "\n")))))

(global-set-key (kbd "<C-f2>") 'th-shell-popup)
;; ------------------------------------------------------------------

;; insert-markdown-inline-math-block
;; ------------------------------------------------------------------
;; I sometimes use a specialized markdown format, where inline math-blocks can be
;; achieved by surrounding a LaTeX formula with $math$ and $/math$. Writing these out
;; became tedious, so I wrote a small function.

(defun insert-markdown-inline-math-block ()
  "Inserts an empty math-block if no region is active, otherwise wrap a math-block around the region."
  (interactive)
  (let* ((beg (region-beginning))
         (end (region-end))
         (body (if (region-active-p) (buffer-substring beg end) "")))
    (when (region-active-p)
      (delete-region beg end))
    (insert (concat "$math$ " body " $/math$"))
    (search-backward " $/math$")))

;; Most of my writing in this markup is in Norwegian, so the dictionary is set accordingly.
;; The markup is also sensitive to line breaks, so auto-fill-mode is disabled.
;; Of course we want to bind our lovely function to a key!
(add-hook 'markdown-mode-hook
  (lambda ()
    (auto-fill-mode 0)
    (visual-line-mode 1)
    (ispell-change-dictionary "norsk")
    (local-set-key (kbd "C-c m b") 'insert-markdown-inline-math-block)) t)
;; ------------------------------------------------------------------

;; Occur
;; ------------------------------------------------------------------
;; Occur is awesome. Do M-s o to search for a word in current buffer and show list of all occurrences in a separate buffer.
;; M-x   multi-occur-in-matching-buffers
;; is where the real power is, since this lets you search all open buffers matching the
;; regexp you give it, and show hits in a new buffer, which behaves just like the compile buffer.

; (eval-when-compile
;   (require 'cl))

(defun get-buffers-matching-mode (mode)
  "Return a list of buffers where their 'major-mode' is equal to MODE."
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))
; (global-set-key (kbd "C-<f4>") 'multi-occur-in-this-mode)
;; ------------------------------------------------------------------

;; 拷贝代码自动格式化
;; ------------------------------------------------------------------
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
;; ------------------------------------------------------------------

;; Transparency
;; ------------------------------------------------------------------
; (set-frame-parameter (selected-frame) 'alpha '(95 . 50))
; (add-to-list 'default-frame-alist '(alpha . (95 . 50)))

; (defun toggle-transparency ()
;     (interactive)
;     (let ((alpha (frame-parameter nil 'alpha)))
;       (set-frame-parameter
;   nil 'alpha
;       (if (eql (cond ((numberp alpha) alpha)
;            ((numberp (cdr alpha)) (cdr alpha))
;            ;; Also handle undocumented (<active> <inactive>) form.
;            ((numberp (cadr alpha)) (cadr alpha)))
;          100)
;     '(95 . 50) '(100 . 100)))))
; (global-set-key (kbd "C-c C-t") 'toggle-transparency)
;; ------------------------------------------------------------------

;; Prompt before closing Emacs
;; ------------------------------------------------------------------
;; http://nileshk.com/2009/06/13/prompt-before-closing-emacs.html.
; (defun ask-before-closing ()
;   "Ask whether or not to close, and then close if y was pressed."
;   (interactive)
;   (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
;       (if (< emacs-major-version 22)
;       (save-buffers-kill-terminal)
;     (save-buffers-kill-emacs))
;     (message "Exit canceled")))
; (global-set-key (kbd "C-x C-c") 'ask-before-closing)
;; ------------------------------------------------------------------


;; ==================================================================
;; KEYMAPS CONFIGURATION
;; ==================================================================

;; Basic Shortcut - CONFIG
;; ------------------------------------------------------------------

(defalias 'yes-or-no-p 'y-or-n-p)  ;; Type y/n instead of yes/no

;; Use Alt-SPC to set-mark, not the default C-@
(global-set-key (kbd "M-<SPC>") 'set-mark-command)

;; Use Ctrl-Wheel to change the text font size, for Windows.
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; Set C-> to switch/toggle between windows, the default is C-x-o.
(global-set-key (kbd "C->") 'other-window)

;; 设置 Ctrl-Enter 在当前行下方插入新行
(global-set-key (kbd "C-<return>")
    '(lambda ()
        (interactive)
        (move-end-of-line 1)
        (newline)))

;; 设置 Ctrl-Shift-Enter 在当前行上方插入新行
(global-set-key (kbd "C-S-<return>")
    '(lambda ()
        (interactive)
        (move-beginning-of-line 1)
        (open-line 1)))

;; Ctrl-Tab, word-completion, hippie-expand
; (global-set-key [(control tab)] 'hippie-expand)

;; A better C-h i is C-h C-i which has tab compleation on info sections
(define-key 'help-command (kbd "C-i") 'info-display-manual)

;; Programing Shortcut - CONFIG
;; ------------------------------------------------------------------

; (setq-default compile-command "make")
(global-set-key [f5] 'compile)              ;; 编译
; (global-set-key [f6] 'speedbar)           ;; 打开 speedbar

(global-set-key [f8] 'gdb)                  ;; 启动gdb
(global-set-key [C-f8] 'gdb-many-windows)   ;; 启动窗口gdb

; (global-set-key [f9] 'previous-error)
; (global-set-key [f10] 'next-error)

;; Global key-bindings - CONFIG
;; ------------------------------------------------------------------

(global-set-key (kbd "C-x w") 'write-region)   ;; save marked region as a file

(global-set-key "\C-ci" 'indent-region)          ;;Indent row/marked region
; (global-set-key "\C-ca" 'mark-whole-buffer)      ;;same as C-x h
; (global-set-key "\C-ct" 'untabify)               ;;replace all evil TAB to SPC nice!


; (global-set-key (kbd "<f7>")  'toggle-truncate-lines)       ;; line wrapping on/off
; (global-set-key (kbd "<f8>")  'comment-or-uncomment-region) ;; comment/un-comment region


(message "Configuration file read to end!")
(message (concat "/**/: emacs init time, "  ;; show the startup time.
    (substring (emacs-init-time)) "."))


;; ==================================================================
;; THE END!
;; ==================================================================

; (provide 'init)
;;; init.el ends here
