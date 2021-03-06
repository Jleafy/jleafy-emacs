## jleafy-emacs

A simple Emacs configuration project.

### Features

- Start time less than 1 second, light and handy;
- Some tools make the Emacs more easy to be used;
- Many self-defined functions;
- several special mode.

### Installation

You can install jleafy-emacs via the command line (root):

> git clone http://github.com/Jleafy/jleafy-emacs.git ~/.emacs.d

### List of Used Tools

#### Basic Packages

- [**use-package**](https://github.com/jwiegley/use-package): Manage your packages and isolate package configuration.

- [**swiper**](https://github.com/abo-abo/swiper): Ivy - a generic completion frontend for Emacs, Swiper - isearch with an overview, and more.

- [**which-key**](https://github.com/justbur/emacs-which-key#which-key): Emacs package that displays available keybindings in popup.

- [**avy**](https://github.com/abo-abo/avy): Jump to things in Emacs tree-style.

- [**quickrun**](https://github.com/syohex/emacs-quickrun): Run command quickly, surport many kinds of languages.

- [**flycheck**](https://github.com/flycheck/flycheck/): On the fly syntax checking for GNU Emacs.

- [**auto-complete**](https://github.com/auto-complete/auto-complete): An Intelligent auto-completion extension for Emacs.

- [**multiple-cursors**](https://github.com/magnars/multiple-cursors.el): Multiple cursors for emacs.

- [**sr-speedbar**](https://github.com/emacsmirror/emacswiki.org/blob/master/sr-speedbar.el): A mode that makes SpeedBar show in the Current Frame.

- [**volatile-highlights**](https://github.com/k-talo/volatile-highlights.el): Minor mode for visual feedback on some operations in Emacs.

- [**highlight-symbol**](https://github.com/nschum/highlight-symbol.el): Automatic and manual symbol highlighting for Emacs.

- [**indent-guide**](https://github.com/zk-phi/indent-guide): Show vertical lines to guide indentation.

- [**undo-tree**](https://github.com/emacsmirror/undo-tree): Treat undo history as a tree.

- [**anzu**](https://github.com/syohex/emacs-anzu): Provides a minor mode which displays current match and total matches information in the mode-line in various search modes.

- [**benchmark-init**](https://github.com/dholm/benchmark-init-el): Benchmark your Emacs initialization, which can be used to keep track of where time is being spent during Emacs startup in order to optimize startup times.

#### Special Mode

- [**markdown-mode**](https://github.com/defunkt/markdown-mode): A major mode for editing Markdown-formatted text.

- [**pylint**](https://www.pylint.org/): Syntax checker for python.

- [**go-mode**](https://github.com/dominikh/go-mode.el): Emacs mode for the Go programming language.

#### Some other usefull packages(not used)

- [**smex**](https://github.com/nonsequitur/smex): A smart M-x enhancement for Emacs.

- [**ivy**](https://github.com/abo-abo/swiper): A generic completion frontend for Emacs, similar to ido or helm.

- [**yasnippets**](https://github.com/joaotavora/yasnippet): A template system for Emacs, which allows you to type an abbreviation and automatically expand it into function templates.

- [**auctex**](http://www.gnu.org/software/auctex/): AUCTEX is an extensible package for writing and formatting TEX files in GNU Emacs and XEmacs.

- [**expand-region**](https://github.com/magnars/expand-region.el): Emacs extension to increase selected region by semantic units.

### Tips

#### User-defined Keymap

##### General

Keybinding             | Description
-----------------------|------------------------------------------------------------
<kbd>M-/</kbd>         | Run `hippie-expand` (a replacement for the default `dabbrev-expand`).
<kbd>C-c C-t</kbd>     | `me/insert-current-time`
<kbd>C-c C-d</kbd>     | `me/insert-current-date`
<kbd>C-F1</kbd>        | Lookup word online (`me/lookup-word-definition`).
<kbd>C-M-F3</kbd>      | Multi occur, just like M-s o (`me/multi-occur-in-this-mode`).
<kbd>M-[</kbd>         | Unindent region (`me/untab-region`).
<kbd>M-]</kbd>         | Indent region (`me/tab-region`).
<kbd>M-;</kbd>         | Comment Enhanced (`me/enhance-comment-dwim-line`).
<kbd>C-Enter</kbd>     | Open new line below current line with indentation (`me/open-line-below`).
<kbd>C-S-Enter</kbd>   | Open new line above current line with indentation (`me/open-line-above`).
<kbd>C-M-Up</kbd>      | Move region/current line ARG lines up (`me/move-text-up`).
<kbd>C-M-Down</kbd>    | Move region/current line ARG lines down (`me/move-text-down`).
<kbd>C-F11</kbd>       | Change the transparency of emacs (`me/toggle-loop-transparency`).
<kbd>F11</kbd>         | Toggle fullscreen (`me/toggle-fullscreen`).
<kbd>M-@</kbd>         | Remap mark-word (`me/mark-word`).
<kbd>C-S-l</kbd>       | Mark current line (`me/mark-line`).
<kbd>C-M-l</kbd>       | Duplicate current Line forward (`me/duplicate-forward`).
<kbd>C-Backspace</kbd> | Kill to the beginning of the line (`me/backward-kill-line`).
<kbd>S-Mouse1</kbd>    | Enable Emacs column selection using mouse (`me/mouse-start-rectangle`).
<kbd>C-^</kbd>         | Join two lines into one (with the next line).
<kbd>M-Up/Down</kbd>   | Scroll a line up/down every time.
<kbd>C-S-F5</kbd>      | Start compile (`compile`).
<kbd>F8</kbd>          | Start gdb (`gdb`).
<kbd>C-F8</kbd>        | Start multi-windows gdb (`gdb-many-windows`).
<kbd>C-h C-i</kbd>     | A better C-h i (`info-display-manual`).
<kbd>C-x w</kbd>       | Save marked region as a file (`write-region`).
<kbd>C-F12</kbd>       | Line wrapping on/off (`toggle-truncate-lines`).
<kbd>C-S-x</kbd>       | Cut, same as C-w (`kill-region`).
<kbd>C-S-c</kbd>       | Copy, same as M-w (`kill-ring-save`).
<kbd>C-S-v</kbd>       | Past, same as C-y (`yank`).
<kbd>M-p</kbd>         | `backward-paragraph`
<kbd>M-n</kbd>         | `forward-paragraph`

##### Specific Packages

Keybinding             | Description
-----------------------|------------------------------------------------------------
<kbd>C-s</kbd>         | swiper (`swiper`).
<kbd>C-:</kbd>         | avy (`avy-goto-char`).
<kbd>M-g w</kbd>       | avy (`avy-goto-word-2`).
<kbd>F5</kbd>          | quickrun (`quickrun`).
<kbd>C-F5</kbd>        | quickrun (`quickrun-shell`).
<kbd>C-Tab</kbd>       | auto-complete (`auto-complete`).
<kbd>F6</kbd>          | sr-speedbar (`sr-speedbar-toggle`).
<kbd>C-S-d</kbd>       | multiple-cursors (`mc/mark-next-like-this-word`).
<kbd>M-S-down</kbd>    | multiple-cursors (`mc/mark-next-like-this`).
<kbd>M-S-up</kbd>      | multiple-cursors (`mc/mark-previous-like-this`).
<kbd>C-c m r</kbd>     | multiple-cursors (`set-rectangular-region-anchor`).
<kbd>C-c m c</kbd>     | multiple-cursors (`mc/edit-lines`).
<kbd>C-c m a</kbd>     | multiple-cursors (`mc/edit-beginnings-of-lines`).
<kbd>C-c m e</kbd>     | multiple-cursors (`mc/edit-ends-of-lines`).
<kbd>C-M-Mouse1</kbd>  | multiple-cursors (`mc/add-cursor-on-click`).
<kbd>C-F3</kbd>        | highlight-symbol (`highlight-symbol`).
<kbd>F3</kbd>          | highlight-symbol (`highlight-symbol-next`).
<kbd>S-F3</kbd>        | highlight-symbol (`highlight-symbol-prev`).
<kbd>M-F3</kbd>        | highlight-symbol (`highlight-symbol-query-replace`).
<kbd>M-%</kbd>         | anzu (`anzu-query-replace`).
<kbd>C-M-%</kbd>       | anzu (`anzu-query-replace-regexp`).
<kbd>C-c l</kbd>       | org-mode (`org-store-link`).
<kbd>C-c c</kbd>       | org-mode (`org-capture`).
<kbd>C-c a</kbd>       | org-mode (`org-agenda`).
<kbd>C-c b</kbd>       | org-mode (`org-iswitchb`).


### Report bug
