;;; init.el --- My Emacs configuration

;; Author: Jleafy
;; Date: 2017-10-18
;; Version: 0.2.1
;; From: http://home.thep.lu.se/~karlf/emacs.html
;; Time-stamp: <Last changed 2017-10-18 9:43:17 by Jleafy, Jleafy>

;;; Commentary:
;; Following lines load an Org file and build the configuration code out of it.

;;; Code:


; (let ((gc-cons-threshold most-positive-fixnum))
(let ((file-name-handler-alist nil))

;; Garbage-collect on focus-out, Emacs should feel snappier.
; (setq gc-cons-threshold 100000000)
(setq gc-cons-threshold most-positive-fixnum)

(message "Reading configuration file ...")

;; ------------------------------------------------------------------
;; => Basic UI
;; ------------------------------------------------------------------
;; Disable menu bars, etc.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode 0)

;; Windows Size
; (setq default-frame-alist '((height . 35) (width . 120)))
; (setq initial-frame-alist '((height . 37) (width . 122)))
; (setq initial-frame-alist (quote ((fullscreen . maximized))))  ;; fullscreen on Windows

;; Font
(when (display-graphic-p)
  (defconst en-font-size 14)
  (defconst en-font-list
    '("Consolas"
      "Courier New"
      "DejaVu Sans Mono"
      "Bitstream Vera Sans Mono"))
  (defconst cn-font-size 14)
  (defconst cn-font-list
    '("Microsoft Yahei"
      "WenQuanYi Micro Hei Mono"
      "WenQuanYi Zen Hei Mono"
      "Courier New"))
  (defun find-font (fonts)
    (catch 'return
      (dolist (ft fonts)
    (if (x-list-fonts ft)
        (throw 'return ft)))))
  (defun set-font (font size lang)
    (pcase lang
      (`en (set-default-font (format "%s-%s" font size)))
      (`cn (set-fontset-font t 'han (format "%s-%s" font size)))
      (lang (message "Unknow lang %s" lang))))
  (set-font (find-font en-font-list) en-font-size 'en)
  (set-font (find-font cn-font-list) cn-font-size 'cn))

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/themes")
(load-theme 'atom-one-dark t)
; (load-theme 'Amelie t)
; (load-theme 'dracula t)
; (load-theme 'idea-darkula t)
; (load-theme 'flatland t)
; (load-theme 'smyx t)
; (load-theme 'seti t)
; (load-theme 'subdued t)
; (load-theme 'twilight t)
; (load-theme 'tangotango t)
; (load-theme 'zenburn t)

;; White theme
; (load-theme 'FlatUI t)
; (load-theme 'github t)
; (load-theme 'github-modern t)
; (load-theme 'material-light t)

; Here's list of emacs 24.3 themes.
; (load-theme 'deeper-blue)
; adwaita       -- white
; dichromacy    -- white
; whiteboard    -- white
; deeper-blue


;; ------------------------------------------------------------------
;; => User Info and Path
;; ------------------------------------------------------------------
;; user info
(setq user-full-name "Jleafy")
(setq user-mail-address "jleafy@163.com")

;; change default directory
(setq default-directory "~/Desktop/")
(setq command-line-default-directory "~/Desktop/")

;; load file and path
; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load .custom.el
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Make directory tmp
(defvar tmp-directory (concat user-emacs-directory "tmp"))
(if (not (file-exists-p tmp-directory))
    (make-directory tmp-directory t))


;; ------------------------------------------------------------------
;; => User better default
;; ------------------------------------------------------------------
;;; Here are what I consider better defaults as per my own experience.
(setq-default
  ; blink-cursor-mode 0                         ;; Disable the cursor blinking
  ; left-margin-width 1 right-margin-width 1    ;; Add left and right margins
  ad-redefinition-action 'accept              ;; Silence warnings for redefinition
  column-number-mode t                        ;; Show colum number on minibuffer
  cursor-in-non-selected-windows t            ;; Hide the cursor in inactive windows
  cursor-type '(bar . 1)                      ;; Modify the cursor style
  delete-by-moving-to-trash t                 ;; Delete files to trash
  echo-keystrokes 0.3                         ;; Echo commands I haven't finished quicker than the default of 1 second
  fill-column 80                              ;; Set width for automatic line breaks
  help-window-select t                        ;; Focus new help windows when opened
  indent-tabs-mode nil                        ;; Stop using tabs to indent
  inhibit-startup-screen t                    ;; Disable start-up screen
  kill-whole-line t                           ;; Kill whole line when used C-k at the beginning of a line
  line-number-mode t                          ;; Show row number on minibuffer
  mode-require-final-newline 'visit           ;; Add a newline at EOF on visit
  mouse-avoidance-mode 'banish                ;; Avoid collision of mouse with point
  mouse-wheel-mode t
  mouse-yank-at-point t                       ;; Yank at point rather than pointer
  ns-use-srgb-colorspace nil                  ;; Don't use sRGB colors
  recenter-positions '(5 top bottom)          ;; Set re-centering positions
  resize-mini-windows t                       ;; Allow minibuffer auto resize
  select-enable-clipboard t                   ;; Merge system's and Emacs' clipboard
  sentence-end-double-space nil               ;; End a sentence after a dot and a space
  show-paren-mode t                           ;; turn on bracket match highlight
  show-paren-style 'parenthesis
  split-height-threshold nil                  ;; Disable vertical window splitting
  split-width-threshold nil                   ;; Disable horizontal window splitting
  tab-width 4                                 ;; Set width for tabs
  window-combination-resize t                 ;; Resize windows proportionally
  x-stretch-cursor t                          ;; Stretch cursor to the glyph width
  message-log-max t)                          ;; Keep message buffer complete

; (fringe-mode 0)                               ;; Hide fringes
(delete-selection-mode)                       ;; Replace region when inserting text
(global-hl-line-mode)                         ;; Hightlight current line
(global-subword-mode)                         ;; Iterate through CamelCase words
(put 'downcase-region 'disabled nil)          ;; Enable downcase-region
(put 'upcase-region 'disabled nil)            ;; Enable upcase-region
(auto-image-file-mode t)                      ;; Enable to open image
(electric-indent-mode 1)                      ;; Make Return key also do indent globally
(file-name-shadow-mode t)                     ;; Be smart about file names in mini buffer
(global-auto-revert-mode 1)                   ;; Refresh file automatically
(global-font-lock-mode t)                     ;; Syntax on
(global-prettify-symbols-mode 1)              ;; Beautify display symbols(elisp), for example, "lambda" show as "λ"
(transient-mark-mode t)                       ;; Highlight selected region

(setq major-mode 'text-mode)
(setq next-line-add-newlines nil)             ;; Don't add a newline when the cursor moved to next line
(setq Man-notify-method 'pushy)               ;; When reading man-doc, use current buffer
(setq font-lock-maximum-decoration t)         ;; Adds pretty colors.
(setq read-file-name-completion-ignore-case t);; Ignore case when using completion for file names
(setq system-uses-terminfo nil)               ;; Fix some weird color escape sequences
; (setq use-file-dialog nil)
; (setq use-dialog-box nil)
; (setq pop-up-frames t)                      ;; each file opens in a new window
; (cua-mode 1)                                ;; make {copy, cut, paste, undo} have {C-c, C-x, C-v, C-z} keys.

;;; yes/no
; (setq confirm-kill-emacs 'yes-or-no-p)        ;; Confirm before exiting Emacs
(defalias 'yes-or-no-p 'y-or-n-p)             ;; Replace yes/no prompts with y/n

;;; Line Number
(global-linum-mode 'linum-mode)               ;; Always show line numbers
; (setq linum-format " %d ")                    ;; set format

;;; Scroll margin
(setq-default scroll-step 1 scroll-margin 3 scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)
(setq kill-ring-max 200)

;;; Encoding UTF-8
; (set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq file-name-coding-system 'gbk)
(set-terminal-coding-system 'utf-8)

;;; Title
(setq frame-title-format "emacs@%b")          ;; show current filename at titlebar
; (setq frame-title-format '(buffer-file-name "%f" ("%b"))) ;; titlebar =buffer unless filename

;;; Initial Scratch Message
(setq initial-scratch-message "")             ;; Empty the initial *scratch* buffer
; (setq initial-scratch-message
;   (concat ";; scratch buffer created, welcome to "
;     (substring (emacs-version) 0 16) ".\n"))

;;; Whitespace
(setq-default show-trailing-whitespace t)     ;; Display trailing whitespaces
(add-hook 'before-save-hook 'whitespace-cleanup)

;;; Show-Time
(display-time-mode t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

;;; Time-stamp
;; when there is a "Time-stamp: <>" in the first 10 lines of the file,
;; emacs will write time-stamp information there when saving the file.
(setq time-stamp-active t          ;; do enable time-stamps
  time-stamp-line-limit 10         ;; check first 10 buffer lines for Time-stamp: <>
  ; time-stamp-format "%:u %02m/%02d/%04y %02H02M02S"
  time-stamp-format "Last changed %04y-%02m-%02d %02H:%02M:%02S by %L, %u")  ;; date format
(add-hook 'write-file-hooks 'time-stamp)  ;; update when saving

;;; Uniquify Buffers
(setq uniquify-buffer-name-style 'post-forward
  uniquify-separator ":"
  uniquify-after-kill-buffer-p t       ;; rename after killing uniquified
  uniquify-ignore-buffers-re "^\\*")   ;; don't muck with special buffers

;;; Save mode-line History
(savehist-mode t)
(setq savehist-additional-variables '(search-ring regexp-search-ring))
(setq savehist-file (expand-file-name "tmp/.savehist" user-emacs-directory))

;;; Bookmarks
;; C-x r m ('make'): create a new bookmark,
;; C-x r b ('bookmark'): jump to an existing bookmark,
;; C-x r l ('list'): see the list of your bookmarks.
(setq bookmark-save-flag 1)  ;; auto save changes
(setq bookmark-default-file (expand-file-name "tmp/.bookmarks" user-emacs-directory))

;;; Backup and Auto-save
; (defvar backup-directory (concat user-emacs-directory "tmp/backups"))
; (if (not (file-exists-p backup-directory))
;     (make-directory backup-directory t))
; ;; Sets all files to be backed up and auto saved in a single directory.
; (setq backup-directory-alist `((".*" . ,backup-directory))
;       auto-save-file-name-transforms `((".*" ,backup-directory t)))
; (setq make-backup-files t          ;; backup of a file the first time it is saved.
;       backup-by-copying t          ;; don't clobber symlinks
;       version-control t            ;; version numbers for backup files
;       kept-new-versions 2
;       kept-old-versions 5
;       delete-old-versions t        ;; delete excess backup files silently
;       delete-by-moving-to-trash t)
(setq make-backup-files nil)         ;; No annoying "~file.txt"
(setq auto-save-default nil)         ;; no auto saves to #file#

;;; Abbrevs
; (setq save-abbrevs t)                      ;; (ask) save abbrevs when files are saved
; (setq-default abbrev-mode t)               ;; turn it on for all modes
; (setq abbrev-file-name (expand-file-name "tmp/.abbrev" user-emacs-directory))
; (when (file-exists-p abbrev-file-name)
;   (quietly-read-abbrev-file))              ;; don't tell
; (add-hook 'kill-emacs-hook 'write-abbrev-file)  ;; write when exiting emacs


;; ------------------------------------------------------------------
;; => Hook list
;; ------------------------------------------------------------------
;; Make URLs in comments/strings clickable
(add-hook 'find-file-hooks 'goto-address-prog-mode)
;; Make shell scrips executable on save. Good!
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
;; When compiling from shell, display error result as in compilation buffer, with links to errors.
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)


;; ==================================================================
;; Initialization for Packages
;; ==================================================================

;; Set repositories
(require 'package)
(setq-default
  load-prefer-newer t
  package-enable-at-startup nil)
(setq package-archives '(
  ;; mirrors in China
  ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
  ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
  ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
  ))
(package-initialize)

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
; (eval-when-compile (require 'use-package))
(require 'use-package)
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(setq-default
  ; use-package-verbose t
  ; use-package-always-defer t
  use-package-always-ensure t)


;; ==================================================================
;; PLUGINS CONFIGURATION
;; ==================================================================

;; molokai-theme-theme
; (use-package molokai-theme
;   :init
;   (load-theme 'molokai t))

;; sanityinc-tomorrow-theme
; (use-package color-theme-sanityinc-tomorrow
;   :init
;   (load-theme 'sanityinc-tomorrow-night t))

;; spacemacs-theme
; (use-package spacemacs-theme
;   :init
;   (load-theme 'spacemacs-dark t))

;; Start server
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; Make directory server
(defvar server-directory (concat user-emacs-directory "server"))
(if (not (file-exists-p server-directory))
  (make-directory server-directory t))

;; Windows platform Emacs single instance settings and right-click menu added
;; "D:\Tools\emacs\bin\emacsclientw.exe" --no-wait --alternate-editor="D:\Tools\emacs\bin\runemacs.exe" "%1"
; (require 'server)
; (unless (server-running-p)
;   (server-start))


;; ------------------------------------------------------------------
;; => Built-In Packages
;; ------------------------------------------------------------------
;;; hippie-exp
(use-package hippie-exp
  :defer t
  :config
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
  :bind
  ("M-/" . hippie-expand))

;;; recentf
(use-package recentf
  :defer t
  :config
  (recentf-mode 1)
  (setq recentf-save-file (expand-file-name "tmp/.recentf" user-emacs-directory))
  (setq-default
    recentf-max-saved-items 100
    recentf-exclude '("/tmp/" "/ssh:"))
    ; recentf-auto-cleanup 600)
  :bind
  ("C-x C-r" . recentf-open-files))

;;; saveplace
(use-package saveplace
  :defer t
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name "tmp/.places" user-emacs-directory)))

;;; dired/dire-x
(use-package dired
  :ensure nil
  :config
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (defadvice dired-readin (after dired-after-updating-hook first () activate)
    "Sort dired listings with directories first before adding marks."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2)
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))
  ; (put 'dired-find-alternate-file 'disabled nil)   ;; set only one buffer for dired mode
  (setq-default
    dired-auto-revert-buffer t
    dired-hide-details-hide-symlink-targets nil
    dired-listing-switches "-alh"
    dired-ls-F-marks-symlinks nil
    ; dired-recursive-deletes 'always
    dired-recursive-copies 'always))

(use-package dired-x
  :ensure nil
  :preface
  (defun me/dired-revert-after-command (command &optional output error)
    (revert-buffer))
  :config
  (advice-add 'dired-smart-shell-command :after #'me/dired-revert-after-command))

;;; winner-mode
;; Turn on winner-mode, which allows me to use C-c LEFT to undo window configuration changes, if so desired.
(use-package winner
  :defer t
  :init
  (winner-mode 1))

;;; whitespace
(use-package whitespace
  :ensure nil
  :defer t
  :config
  (add-hook 'prog-mode-hook #'whitespace-turn-on)
  (add-hook 'text-mode-hook #'whitespace-turn-on)
  (setq-default whitespace-style '(empty tab trailing)))


;; ------------------------------------------------------------------
;; => External Packages
;; ------------------------------------------------------------------
;;; smex
(use-package smex
  :diminish smex-mode
  :init
  (smex-initialize)
  (setq smex-save-file (expand-file-name "tmp/.smex-items" user-emacs-directory)))

;;; ivy
(use-package ivy
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq enable-recursive-minibuffers t)
  ; (setq ivy-height 10)        ;; number of result lines to display
  (setq ivy-count-format "")  ;; does not count candidates
  (setq ivy-display-style 'fancy)
  (setq ivy-format-function 'ivy-format-function-line) ; Make highlight extend all the way to the right
  (setq ivy-initial-inputs-alist nil)  ;; no regexp by default
  ; (setq ivy-initial-inputs-alist '((counsel-M-x . "^") (man . "^") (woman . "^")))
  ;; configure regexp engine
  (setq ivy-re-builders-alist
      '((counsel-M-x . ivy--regex-fuzzy) ; Only counsel-M-x use flx fuzzy search
        (t . ivy--regex-ignore-order)  ; allow input not in order
        (t . ivy--regex-plus)))
  (add-hook 'after-init-hook
    (lambda ()
      (when (bound-and-true-p ido-ubiquitous-mode)
          (ido-ubiquitous-mode -1))
      (when (bound-and-true-p ido-mode)
          (ido-mode -1))
          (ivy-mode 1)))
  :bind
  (:map ivy-minibuffer-map        ; bind in the ivy buffer
    ("RET" . ivy-alt-done)
    ("C-!" . ivy-immediate-done)
    ("C-+" . ivy-call)
    ("C-<" . ivy-avy)
    ("C->" . ivy-dispatching-done)
    ("C-[" . ivy-previous-history-element)
    ("C-]" . ivy-next-history-element))
  )

;;; yasnippet
(use-package yasnippet
  :diminish yasnippet-mode
  :config
  ; (yas-global-mode 1)
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

;;; flycheck
(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
  (setq-default
    flycheck-check-syntax-automatically '(save mode-enabled)
    flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)
    flycheck-display-errors-delay .3)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

;;; auto-complete
(use-package auto-complete
  :defer t
  :diminish auto-complete-mode
  :config
  (ac-config-default)
  (setq ac-dwim t)                     ;; tab-mode
  (setq ac-use-quick-help nil
        ; setq ac-quick-help-delay 1.0
        ac-auto-start 2                ;; auto start to complete after input 3 char
        ac-auto-show-menu 0.5          ;; Show menu 0.8 second later
        ac-menu-height 12              ;; menu seted as 12 lines
        ac-use-menu-map t)             ;; use menu map
  (setq ac-comphist-file (expand-file-name "tmp/.ac-comphist" user-emacs-directory))
  ;; When typed char was deleted, you can still toggle ac-completion
  (setq ac-trigger-commands
        (cons 'backward-delete-char-untabify ac-trigger-commands))
  :bind
  (("<C-tab>" . auto-complete)
    :map ac-menu-map
    ("C-n" . ac-next)
    ("C-p" . ac-previous)))

;;; autopair
(use-package autopair
  :diminish autopair-mode
  :config
  (autopair-global-mode))


;;; multiple-cursors
(use-package multiple-cursors
  :defer t
  :config
  (setq-default mc/list-file (expand-file-name "tmp/.mc-lists.el" user-emacs-directory))
  (setq-default
    mc/edit-lines-empty-lines 'ignore
    mc/insert-numbers-default 1)
  :bind
  (("C-S-<mouse-1>" . mc/add-cursor-on-click)
   ("C-S-d" . mc/mark-next-like-this-word)
   ("M-S-<down>" . mc/mark-next-like-this)
   ("M-S-<up>" . mc/mark-previous-like-this)
   ("C-c m r" . set-rectangular-region-anchor)
   ("C-c m c" . mc/edit-lines)
   ("C-c m a" . mc/edit-beginnings-of-lines)
   ("C-c m e" . mc/edit-ends-of-lines)
   ("C-'" . mc-hide-unmatched-lines-mode)))

;;; sr-speedbar
(use-package sr-speedbar
  :defer t
  :config
  (setq sr-speedbar-right-side nil
        speedbar-show-unknown-files t
        ; setq sr-speedbar-width 30
        speedbar-use-images nil)
  :bind
  ("<f6>" . sr-speedbar-toggle))

;;; highlight-symbol
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :bind
  (("C-<f3>" . highlight-symbol)
   ("<f3>" . highlight-symbol-next)
   ("S-<f3>" . highlight-symbol-prev)
   ("M-<f3>" . highlight-symbol-query-replace)))

;;; which-key
;; show sub-keys after a second of an unfinished key-press
(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode))

;;; indent-guide
(use-package indent-guide
  :diminish indent-guide-mode
  :config
  (indent-guide-global-mode))

;;; undo-tree
;; Tips for uesage: C-x u - undo-tree-visualizer-mode; p/n - move up/down;
;; b/f - switch between left branch and right; t - show time-stamp; q - exit.
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))

;;; browse-kill-ring
;; interactively insert items from kill-ring.
(use-package browse-kill-ring
  :diminish browse-kill-ring-mode
  :config
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-quit-action 'save-and-restore))

;;; volatile-highlights
;; minor mode for visual feedback on some operations.
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;;; benchmark-init
;; keep track of where time is being spent during Emacs startup.
(use-package benchmark-init
  :diminish benchmark-init-mode
  :config
  (benchmark-init/activate))

;;; expand-region
;; Increase region by semantic units.
(use-package expand-region
  :defer t
  :bind
  ("C-+" . er/contract-region)
  ("C-=" . er/expand-region))

;;; anzu
;; displays current match and total matches information in the mode-line in various search modes.
(use-package anzu
  :defer t
  :diminish anzu-mode
  :config
  (global-anzu-mode +1)
  (progn
    (set-face-attribute 'anzu-mode-line nil :foreground "yellow" :weight 'bold)
    (custom-set-variables
      '(anzu-mode-lighter "")
      '(anzu-deactivate-region t)
      '(anzu-search-threshold 1000)
      '(anzu-replace-to-string-separator " => ")))
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)))

;;; flyspell
; (use-package flyspell
;   :diminish flyspell-mode
;   :defer t
;   :config
;   (add-hook 'prog-mode-hook #'my/enable-flyspell-prog-mode)
;   (defun my/enable-flyspell-prog-mode ()
;     (interactive)
;     (flyspell-prog-mode)))


;; ------------------------------------------------------------------
;; => Special Mode
;; ------------------------------------------------------------------

;; ------------------------------------------------------------------
;;; org-mode
(use-package org
  :defer t
  :config
  (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))  ;; auto wrap
  (setq org-src-fontify-natively t  ;; org-mode Syntax on
        org-log-done t
        org-ellipsis "…"            ;; replace the "..." with "…" for collapsed org-mode content
        org-return-follows-link t)  ;; RET follows hyperlinks in org-mode:
  :bind
  (("\C-cl" . org-store-link)
   ("\C-cc" . org-capture)
   ("\C-ca" . org-agenda)
   ("\C-cb" . org-iswitchb)))

;; ------------------------------------------------------------------
;;; markdown-mode
(use-package markdown-mode
  :defer t
  :mode
  (("\\.text\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)))

;;; pandoc-mode
; (use-package pandoc-mode
;   :config
;   (add-hook 'markdown-mode-hook 'pandoc-mode))

;; ------------------------------------------------------------------
;;; LaTex / AucTeX
(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)
  (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource")))

;; ------------------------------------------------------------------
;;; python
;; The package is "python" but the mode is "python-mode":
(use-package python
  :defer t
  :mode
  ("\\.py\\'" . python-mode)
  :interpreter
  ("python" . python-mode)
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args ""
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        python-shell-completion-setup-code
        "from IPython.core.completerlib import module_completion"
        python-shell-completion-module-string-code
        "';'.join(module_completion('''%s'''))\n"
        python-shell-completion-string-code
        "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
  (setq py-electric-colon-active t)
  (setenv "LC_CTYPE" "UTF-8"))

;;; pylint-mode
(use-package pylint
  :after python
  :config
  (add-hook 'python-mode-hook 'pylint-add-menu-items)
  (add-hook 'python-mode-hook 'pylint-add-key-bindings))

;; ------------------------------------------------------------------
;;; matlab-mode
(use-package matlab-mode
  :ensure nil
  :defer t
  :mode
  ("\\.m$" . matlab-mode)
  :config
  (matlab-cedet-setup))

;; ------------------------------------------------------------------
;;; CMake
(use-package cmake-mode
  :ensure nil
  :defer t
  :config
  ;; Add cmake listfile names to the mode list.
  (setq auto-mode-alist
    (append
      '(("CMakeLists\\.txt\\'" . cmake-mode))
      '(("\\.cmake\\'" . cmake-mode))
      auto-mode-alist)))

;; ------------------------------------------------------------------
;;; GUD Mode (gdb debugging)
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
                         (forward-line 1)))))
  (setq gdb-many-windows t))

;; ------------------------------------------------------------------
;;; C++ Mode

(setq compilation-ask-about-save nil)  ;; don't ask me to save _all_ buffers
(setq compilation-scroll-output 'first-error)  ;; Stop on the first error
(setq compilation-skip-threshold 2)  ;; Don't stop on info or warnings

;; 1. Generic
;; C++-specific. Which extensions should be associated with C++ (rather than C)
(add-to-list 'auto-mode-alist '("\\.h$"  . c++-mode)) ;h-files
(add-to-list 'auto-mode-alist '("\\.icc" . c++-mode)) ;implementation files
(add-to-list 'auto-mode-alist '("\\.tcc" . c++-mode)) ;files with templates

;; 2. Better compile buffer
; (require 'compile)
(add-hook 'c-mode-common-hook
  (lambda ()
    (setq
     compilation-scroll-output 'first-error   ;; scroll until first error
     ;; compilation-read-command nil          ;; don't need enter
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
(add-hook 'c-mode-common-hook (lambda () (which-function-mode t)))

;; 4. Navigate .h och .cpp
;; Now, we can quickly switch between myfile.cc and myfile.h with C-c o.
;; Note the use of the c-mode-common-hook, so it will work for both C and C++.
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;; ------------------------------------------------------------------
;;; Ediff Mode
;; To people unfamiliar with ediff, the functions to try are:
;; ediff-current-file (see changes between current modified file and its saved version),
;; ediff-buffers (diff two different buffers),
;; ediff-files (diff two different files, I mark two files in dired and call this).
(custom-set-variables
  '(ediff-window-setup-function 'ediff-setup-windows-plain)  ;; Don't use strange separate control-window.
  '(ediff-split-window-function 'split-window-horizontally)  ;; Side by side comparison
  '(ediff-diff-options "-w"))   ;; Ignore white space
;; reset the window configuration after ediff is done (winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)


;; ==================================================================
;; SELF-DEFINED FUNCTIONS
;; ==================================================================

;; Open config file quickly
;; ------------------------------------------------------------------
(defun me/open-init-file()
  "To open the init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<C-f12>") 'me/open-init-file)

(defun me/copy-buffer-file-path ()
  "Put current buffer's short path into the kill ring."
  (interactive)
  (when (buffer-file-name)
    (kill-new (f-short (buffer-file-name)))))

(defun me/copy-buffer-file-name ()
  "Put current buffer's base name into the kill ring."
  (interactive)
  (when (buffer-file-name)
    (kill-new (f-filename (buffer-file-name)))))
;; ------------------------------------------------------------------

;; Insert Time and Date
;; ------------------------------------------------------------------
;; Insert time
(defun me/insert-current-time ()
  "Insert the current time."
  (interactive "*")
  ; (insert (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))))
  (insert (format-time-string "%H:%M:%S" (current-time))))
(global-set-key "\C-xt" 'me/insert-current-time)

;; Insert date
(defun me/insert-current-date ()
  "Insert the current date."
  (interactive "*")
  ; (insert (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))))
  (insert (format-time-string "%Y/%m/%d" (current-time))))
(global-set-key "\C-xd" 'me/insert-current-date)

;; Insert the current date.
(defun me/date-iso ()
  "Insert the current date, ISO format, eg. 2016-12-09."
  (interactive)
  (insert (format-time-string "%F")))

(defun me/date-iso-with-time ()
  "Insert the current date, ISO format with time, eg. 2016-12-09T14:34:54+0100."
  (interactive)
  (insert (format-time-string "%FT%T%z")))

(defun me/date-long ()
  "Insert the current date, long format, eg. December 09, 2016."
  (interactive)
  (insert (format-time-string "%B %d, %Y")))

(defun me/date-long-with-time ()
  "Insert the current date, long format, eg. December 09, 2016 - 14:34."
  (interactive)
  (insert (capitalize (format-time-string "%B %d, %Y - %H:%M"))))

(defun me/date-short ()
  "Insert the current date, short format, eg. 2016.12.09."
  (interactive)
  (insert (format-time-string "%Y.%m.%d")))

(defun me/date-short-with-time ()
  "Insert the current date, short format with time, eg. 2016.12.09 14:34"
  (interactive)
  (insert (format-time-string "%Y.%m.%d %H:%M")))
;; ------------------------------------------------------------------

;; Linum-config
;; ------------------------------------------------------------------
;; 1.Linum: fix margins when font is scaled
(defun me/linum-update-window-scale-fix (win)
  "Fix linum margins for scaled text."
  (set-window-margins win
      (ceiling (* (if (boundp 'text-scale-mode-step)
    (expt text-scale-mode-step
        text-scale-mode-amount) 1)
      (if (car (window-margins))
          (car (window-margins)) 1)
      ))))
(advice-add #'linum-update-window :after #'me/linum-update-window-scale-fix)

;; 2.Linum: Select lines by clicking
(defvar *linum-mdown-line* nil)

(defun me/line-at-click ()
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

(defun me/md-select-linum ()
  (interactive)
  (goto-line (me/line-at-click))
  (set-mark (point))
  (setq *linum-mdown-line*
    (line-number-at-pos)))

(defun me/mu-select-linum ()
  (interactive)
  (when *linum-mdown-line*
    (let (mu-line)
      ;; (goto-line (me/line-at-click))
      (setq mu-line (me/line-at-click))
      (goto-line (max *linum-mdown-line* mu-line))
      (set-mark (line-end-position))
      (goto-line (min *linum-mdown-line* mu-line))
      (setq *linum-mdown*
    nil))))

(global-set-key (kbd "<left-margin> <down-mouse-1>") 'me/md-select-linum)
(global-set-key (kbd "<left-margin> <mouse-1>") 'me/mu-select-linum)
(global-set-key (kbd "<left-margin> <drag-mouse-1>") 'me/mu-select-linum)

;; 3.Linum: highlight the current line number
; (require 'linum)
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

(defun me/linum-highlight-current-line (line-number)
  "Highlight the current LINE-NUMBER using 'linum-current-line' face."
  (let ((face (if (= line-number linum-current-line)
                  'linum-current-line
                'linum)))
    (propertize (format (concat "%" linum-border-width "d") line-number)
                'face face)))

(setq linum-format 'me/linum-highlight-current-line)

;; 4.Linum: Separating line numbers from text

(unless window-system
  (add-hook 'linum-before-numbering-hook
        (lambda ()
          (setq-local linum-format-fmt
              (let ((w (length (number-to-string
                        (count-lines (point-min) (point-max))))))
                (concat "%" (number-to-string w) "d"))))))

(defun linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'mode-line)))

(unless window-system
  (setq linum-format 'linum-format-func))

;; ------------------------------------------------------------------

;; Revert buffers
;; ------------------------------------------------------------------
(defun me/revert-all-buffers ()
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
(defun me/search (regexp)
  "Search all buffers for a REGEXP."
  (interactive "sRegexp to search for: ")
  (multi-occur-in-matching-buffers ".*" regexp))
; (global-set-key (kbd "C-c o") 'multi-occur-in-matching-buffers)
;; ------------------------------------------------------------------

;; Nuke all buffers
;; ------------------------------------------------------------------
(defun me/nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapc (lambda (x) (kill-buffer x)) (buffer-list)) (delete-other-windows))
;; ------------------------------------------------------------------

;; Kill all other buffers
;; ------------------------------------------------------------------
(defun me/kill-all-other-buffers ()
  "Kill all other buffers, except the current one."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
;; ------------------------------------------------------------------

;; Delete current buffer and file
;; ------------------------------------------------------------------
;; I like the feel between C-x k to kill the buffer and C-x C-k to kill the file.
;; Release ctrl to kill it a little, hold to kill it a lot.
(defun me/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed." filename)))))
; (global-set-key (kbd "C-c C-k") 'me/delete-current-buffer-file)
;; ------------------------------------------------------------------

;; Word Lookup
;; ------------------------------------------------------------------
(defun me/lookup-word-definition ()
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
(global-set-key (kbd "<C-f1>") 'me/lookup-word-definition)
;; ------------------------------------------------------------------

;; Set open multi shell
;; ------------------------------------------------------------------
(defun me/shell-mode-auto-rename-buffer (text)
  "Set for being able to poen multi shell buffer."
  (if (eq major-mode 'shell-mode)
      (rename-buffer (concat "shell:" default-directory) t)))

(add-hook 'comint-output-filter-functions 'me/shell-mode-auto-rename-buffer)
;; ------------------------------------------------------------------

;; Popup-shell
;; ------------------------------------------------------------------
(defvar my-shell-popup-buffer nil)

(defun me/shell-popup ()
  "Toggle a shell popup buffer with the current file's directory as cwd."
  (interactive)
  (unless (buffer-live-p my-shell-popup-buffer)
    (save-window-excursion (shell "*Popup Shell*"))
    (setq my-shell-popup-buffer (get-buffer "*Popup Shell*")))
  (let ((win (get-buffer-window my-shell-popup-buffer))
        (dir (file-name-directory (or (buffer-file-name)
                                      ;; dired
                                      dired-directory
                                      ;; use HOME
                                      "~/"))))
    (if win
        (quit-window nil win)
      (pop-to-buffer my-shell-popup-buffer nil t)
      (comint-send-string nil (concat "cd " dir "\n")))))

(global-set-key (kbd "<C-f2>") 'me/shell-popup)
;; ------------------------------------------------------------------

;; Multi-occur
;; ------------------------------------------------------------------
;; Occur is awesome. Do M-s o to search for a word in current buffer and show list of all occurrences in a separate buffer.
;; M-x multi-occur-in-matching-buffers
;; is where the real power is, since this lets you search all open buffers matching the
;; regexp you give it, and show hits in a new buffer, which behaves just like the compile buffer.

; (eval-when-compile (require 'cl))
(defun me/get-buffers-matching-mode (mode)
  "Return a list of buffers where their 'major-mode' is equal to MODE."
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun me/multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
    (me/get-buffers-matching-mode major-mode)
    (car (occur-read-primary-args))))

(global-set-key (kbd "C-M-<f3>") 'me/multi-occur-in-this-mode)
;; ------------------------------------------------------------------

;; Tab-Indent
;; ------------------------------------------------------------------
;;*** simple indent/unindent just like other editors
;; unlike emacs' default settings, this would not use syntax-based indent, but:
;;  - if region selected, indent/unindent the region (tab-width)
;;    * the region mark would not deactivated automatically
;;  - if no region selected, <TAB> would
;;    * if cursor lies in line leading, always indent tab-width
;;    * if cursor lies in word ending and `tab-always-indent' is `complete', try complete
;;    * otherwise, always insert a TAB char or SPACEs
;;  - if no region selected, <S-TAB> would
;;    * if cursor lies in line leading, always unindent tab-width
;;    * otherwise, the cursor would move backwards (tab-width)
;; From: https://stackoverflow.com/questions/2249955/emacs-shift-tab-to-left-shift-the-block
;; Note: this implementation would hornor `tab-always-indent', `indent-tabs-mode' and `tab-with'.

(defun me/indent-region-custom(numSpaces)
  (progn
    ;; default to start and end of current line
    (setq regionStart (line-beginning-position))
    (setq regionEnd (line-end-position))

    ;; if there's a selection, use that instead of the current line
    (when (use-region-p)
      (setq regionStart (region-beginning))
      (setq regionEnd (region-end)))

    (save-excursion ;; restore the position afterwards
      (goto-char regionStart) ;; go to the start of region
      (setq start (line-beginning-position)) ;; save the start of the line
      (goto-char regionEnd) ;; go to the end of region
      (setq end (line-end-position)) ;; save the end of the line

      (indent-rigidly start end numSpaces) ;; indent between start and end
      (setq deactivate-mark nil) ;; restore the selected region
    )))

(defun me/untab-region (N)
  "Unindent line, or block if it's a region selected."
  (interactive "p")
  (me/indent-region-custom -4))

(defun me/tab-region (N)
  "Indent line, or block if it's a region selected."
  (interactive "p")
  (if (active-minibuffer-window)
      (minibuffer-complete)    ;; tab is pressed in minibuffer window -> do completion
  ;; else
  (if (string= (buffer-name) "*shell*")
      (comint-dynamic-complete) ;; in a shell, use tab completion
  ;; else
  (if (use-region-p)    ;; tab is pressed is any other buffer -> execute with space insertion
      (me/indent-region-custom 4) ;; region was selected, call indent-region
      (insert "    ") ;; else insert four spaces as expected
  ))))

(global-set-key (kbd "M-[") 'me/untab-region) ;; (kbd "<backtab>")
(global-set-key (kbd "M-]") 'me/tab-region)   ;; (kbd "<tab>")
;; ------------------------------------------------------------------

;; Comment Enhanced
;; ------------------------------------------------------------------
;; If no region is selected and current line is not blank and we are not
;; at the end of the line, then comment current line. Replaces default behaviour of
;; comment-dwim, when it inserts comment at the end of the line.
(defun me/enhance-comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key "\M-;" 'me/enhance-comment-dwim-line)
;; ------------------------------------------------------------------

;; RM-Trailing-Spaces
;; ------------------------------------------------------------------
(defun me/rm-trailing-spaces ()
  "Remove trailing white spaces from the whole document."
  (interactive)
  (save-excursion
    (let ((current (point)))
      (goto-char 0)
      (while (re-search-forward "[ \t]+$" nil t)
        (replace-match "" nil nil))
      (goto-char current)))
  (message "Remove trailing white spaces from the whole document finished."))
;; ------------------------------------------------------------------

;; Opening new lines
;; ------------------------------------------------------------------
(defun me/open-line-below ()
  "Open new line below current line with indentation."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun me/open-line-above ()
  "Open new line above current line with indentation."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'me/open-line-below)
(global-set-key (kbd "<C-S-return>") 'me/open-line-above)
;; ------------------------------------------------------------------

;; Move line/region Up and Down
;; ------------------------------------------------------------------
(defun me/move-text-internal (arg)
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

(defun me/move-text-down (arg)
  "Move region (transient-mark-mode active) or current line ARG lines down."
  (interactive "*p")
  (me/move-text-internal arg))

(defun me/move-text-up (arg)
  "Move region (transient-mark-mode active) or current line ARG lines up."
  (interactive "*p")
  (me/move-text-internal (- arg)))

;; Use Ctrl+Alt+up/down, to move the line under point or region up/down.
(global-set-key [C-M-up] 'me/move-text-up)
(global-set-key [C-M-down] 'me/move-text-down)
;; ------------------------------------------------------------------

;; Show line numbers temporarily
;; ------------------------------------------------------------------
(defun me/goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

; (global-set-key [remap goto-line] 'me/goto-line-with-feedback)
;; ------------------------------------------------------------------

;; Toggles windows
;; ------------------------------------------------------------------
(defun me/toggle-window-split ()
  "This snippet toggles between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
;; ------------------------------------------------------------------

;; Transpose windows
;; ------------------------------------------------------------------
(defun me/transpose-windows ()
  "Transpose your windows, flips a two-window frame, so that left is right, or up is down."
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))
;; ------------------------------------------------------------------

;; Transparency
;; ------------------------------------------------------------------
(setq alpha-list '((95 65) (85 55)(65 35) (100 100)))
(defun me/toggle-loop-transparency ()
  "Loop setting transparent effect."
  (interactive)
  (let ((h (car alpha-list)))  ;; head value will set to
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       ) (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))))

(global-set-key (kbd "<C-f11>") 'me/toggle-loop-transparency)
;; ------------------------------------------------------------------

;; Fullscreen
;; ------------------------------------------------------------------
(defun toggle-fullscreen ()
  "Toggle full screen."
  (interactive)
  (set-frame-parameter
    nil 'fullscreen
    (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(global-set-key (kbd "<f11>") 'toggle-fullscreen)
;; ------------------------------------------------------------------

;; Slick Copy/Kill current line (enhanced C-w or M-w)
;; ------------------------------------------------------------------
;; If nothing is marked/highlighted, and you copy or cut
;; (C-w or M-w) then use column 1 to end. No need to "C-a C-k" or "C-a C-w" etc.

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Killed line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

; (defun slick-cut (beg end)
;   "When called interactively with no active region, kill a single line instead."
;   (interactive
;    (if mark-active
;        (list (region-beginning) (region-end))
;      (message "Killed line")
;      (list (line-beginning-position) (line-beginning-position 2)))))

; (advice-add 'kill-region :before #'slick-cut)

; (defun slick-copy (beg end)
;   "When called interactively with no active region, copy a single line instead."
;   (interactive
;    (if mark-active
;        (list (region-beginning) (region-end))
;      (message "Copied line")
;      (list (line-beginning-position) (line-beginning-position 2)))))

; (advice-add 'kill-ring-save :before #'slick-copy)
;; ------------------------------------------------------------------

;; Search what is selected
;; ------------------------------------------------------------------
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  "When word is selected, search the selected word."
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
;; ------------------------------------------------------------------

;; Mark Current Word
;; ------------------------------------------------------------------
;; We use these for word finding, since the built-in word functions do
;; not include -_. which is annoying, that is we (re-)define word here independent of syntax table.
(defun me/beginning-of-symbol (&optional arg)
  (interactive)
  (backward-word)
  (while (looking-back "[-_\.]")
    (backward-word)))

(defun me/end-of-symbol (&optional arg)
  (interactive)
  (forward-word)
  (while (looking-at "[-_\.]")
    (forward-word)))

(defun me/mark-word()
  "Mark current word."
  (interactive)
  (me/beginning-of-symbol)
  (set-mark-command nil)
  (me/end-of-symbol)
  (setq deactivate-mark nil))

(global-set-key [remap mark-word] 'me/mark-word) ;; default key M-@
;; ------------------------------------------------------------------

;; Mark Current Line
;; ------------------------------------------------------------------
;; If there's already a selection, extend selection downward by line.
;; URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
(defun me/mark-current-line ()
  "Select current line."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun me/mark-line ()
  "Select current line. If region is active, extend selection downward by line."
  (interactive)
  (if (region-active-p)
      (progn
        (forward-line 1)
        (end-of-line))
    (me/mark-current-line)))

(global-set-key (kbd "C-S-l") 'me/mark-line)
;; ------------------------------------------------------------------

;; Duplicate current Line or Region
;; ------------------------------------------------------------------
(defun me/duplicate-line (&optional stay)
  "Duplicate current line. With optional argument STAY true, leave point where it was."
  (save-excursion
    (move-end-of-line nil)
    (save-excursion
      (insert (buffer-substring (point-at-bol) (point-at-eol))))
    (newline))
  (unless stay
    (let ((column (current-column)))
      (forward-line)
      (forward-char column))))

(defun me/duplicate-backward ()
  "Duplicate current line upward or region backward. If region was active, keep it so that the command can be repeated."
  (interactive)
  (if (region-active-p)
      (let (deactivate-mark)
        (save-excursion
          (insert (buffer-substring (region-beginning) (region-end)))))
    (me/duplicate-line t)))

(defun me/duplicate-forward ()
  "Duplicate current line downward or region forward. If region was active, keep it so that the command can be repeated."
  (interactive)
  (if (region-active-p)
      (let (deactivate-mark (point (point)))
        (insert (buffer-substring (region-beginning) (region-end)))
        (push-mark point))
    (me/duplicate-line)))

; (global-set-key (kbd "<M-S-up>") 'me/duplicate-backward)
(global-set-key (kbd "C-M-l") 'me/duplicate-forward)
;; ------------------------------------------------------------------

;; Kill to the beginning of the line
;; ------------------------------------------------------------------
(defun me/backward-kill-line (arg)
  "Kill ARG lines backward, kill the text from the point to the beginning of the line."
  (interactive "p")
  (kill-line (- 1 arg))
  (indent-according-to-mode))

(global-set-key (kbd "C-<backspace>") 'me/backward-kill-line) ;; or "C-S-k"
;; ------------------------------------------------------------------

;; Enable Emacs column selection using mouse
;; ------------------------------------------------------------------
(defun me/mouse-start-rectangle (start-event)
  (interactive "e")
  (deactivate-mark)
  (mouse-set-point start-event)
  (rectangle-mark-mode +1)
  (let ((drag-event))
    (track-mouse
      (while (progn
               (setq drag-event (read-event))
               (mouse-movement-p drag-event))
        (mouse-set-point drag-event)))))

(global-set-key (kbd "S-<down-mouse-1>") 'me/mouse-start-rectangle)
;; ------------------------------------------------------------------

;; Prompt before closing Emacs
;; ------------------------------------------------------------------
(defun me/ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
      (save-buffers-kill-terminal)
    (save-buffers-kill-emacs))
    (message "Exit canceled")))
(global-set-key (kbd "C-x C-c") 'me/ask-before-closing)
;; ------------------------------------------------------------------

;; Auto formatting copied code
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


;; ==================================================================
;; KEYMAPS CONFIGURATION
;; ==================================================================

;; Join adjacent two lines (with the next line)
(global-set-key (kbd "C-S-j") (lambda () (interactive) (join-line -1)))

;; Scroll a line up/down at a time
(global-set-key (kbd "M-<up>")   (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "M-<down>") (lambda () (interactive) (scroll-up 1)))

;; A better C-h i is C-h C-i which has tab compleation on info sections
(define-key 'help-command (kbd "C-i") 'info-display-manual)

(global-set-key [f5] 'compile)              ;; compile
(global-set-key [f8] 'gdb)                  ;; start gdb
(global-set-key [C-f8] 'gdb-many-windows)   ;; start multi-windows gdb
; (global-set-key [f9] 'previous-error)
; (global-set-key [f10] 'next-error)

;; Use Ctrl-Wheel to change the text font size, for Windows.
; (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
; (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(global-set-key (kbd "C-x w") 'write-region)  ;; save marked region as a file
; (global-set-key (kbd "<C-f11>") 'toggle-truncate-lines)  ;; line wrapping on/off

;; User-defined Key
(global-set-key (kbd "C-S-x") 'kill-region)    ;; Cut   same as C-w
(global-set-key (kbd "C-S-c") 'kill-ring-save) ;; Copy  same as M-w
(global-set-key (kbd "C-S-v") 'yank)           ;; Past  same as C-y

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)


;; ------------------------------------------------------------------
;; Message show the startup time
(message (concat "Configuration file read to end!\n=> Emacs init time, "
  (substring (emacs-init-time) 0 10) "."))
)
;;; init.el ends here
