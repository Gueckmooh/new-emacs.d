;; =============================================================================
;; General configuration
;;
;; This file contains the general configuration for emacs, i.e., the elements
;; that are shared and used in any of the modes.
;; =============================================================================
(provide 'general-config)

;; =============================================================================
;; Look and feel configuration
;; =============================================================================
;; Current theme is jetbrains-dracula, some themes that I like are:
;; - sourcerer
;; - flatui
;; - nimbus
;; - material
;; - dracula
;; - gruvbox
(use-package jetbrains-darcula-theme
  :demand t
  :ensure t
  :config
  (load-theme 'jetbrains-darcula t))

;; Highlight the current line in all buffers
(global-hl-line-mode 1)

;; Remove the menu bar, tool bar and scroll bar because they are useless
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Do not display startup screen
(setq-default inhibit-startup-screen t)

;; Configure the scroll step and margin, i.e., the amount of lines that are
;; scrolled when getting at the bottom of the screen and the margin and the
;; margin between the end of the screen and the cursor needed to trigger a
;; scroll
(setq-default scroll-step 1)
(setq-default scroll-margin 7)

;; Display fill column indicator, i.e., the line that tels people that
;; they are doing wrong with their very long line
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
;; Make it look less aggressive to the eye
(set-face-attribute 'fill-column-indicator nil :inherit 'font-lock-comment-face)
(setq-default fill-column 80)

;; Highlight some operations such as undo/redo, kill and suff like that
(use-package volatile-highlights
  :demand t
  :config
  (volatile-highlights-mode t))

;; Draw block cursor as wide as the glyph under it
(setq-default x-stretch-cursor t)

;; Show unncessary whitespace that can mess up your diff
(setq-default show-trailing-whitespace 1)

;; The mode displays the key bindings following your currently entered
;; incomplete command (a prefix) in a popup Display possible keys.
(use-package which-key
  :config
  (which-key-mode))

;; Display matching parenthesis
(show-paren-mode t)

;; Highlight symbol
(use-package auto-highlight-symbol
  :ensure t
  :hook (prog-mode . auto-highlight-symbol-mode)
  :config
  (setq-default ahs-idle-interval 0.5))

;; Display line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; =============================================================================
;; Buffer edition
;; =============================================================================
;; Fancy search in the buffer
(use-package anzu
  :demand t
  :config
  (global-anzu-mode)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "M-ù") 'anzu-query-replace-regexp)
  (set-face-attribute 'anzu-mode-line nil
                      :background "dark magenta" :foreground "white"))

;; Use isearch rather than search, i.e., search with regexps
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Undo tree, the best way to undo/redo stuff
(use-package undo-tree
  :ensure t :demand t
  :config
  (global-undo-tree-mode t))

;; Automatically get reload the buffer if the file changes and force
;; it with C-c C-g
(auto-revert-mode t)
(global-set-key (kbd "C-c C-g") 'revert-buffer)

;; NEVER use tabs to indent your code, it's messy
(setq-default indent-tabs-mode nil)

;; Indent when adding a new line
(global-set-key (kbd "RET") 'newline-and-indent)

;; When killing an end of line, remove the leading exceeding spaces of the next
;; line
(defadvice kill-line (before check-position activate)
  (if (derived-mode-p 'prog-mode)
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

;; C-h is used to delete characters (as in bash) rather than displaying help, f1
;; is used instead
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)

;; Delete the selection when inserting text
(delete-selection-mode)

;; You can click from the terminal
(xterm-mouse-mode)
;; Do not open menu bar when right clicking
(fset 'menu-bar-open nil)
(fset 'x-menu-bar-open nil)

;; Edit files with visual lines not logical lines
;; (global-visual-line-mode t)

;; Yes or No -> y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Everything is utf-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Run keyboard macro
(global-set-key (kbd "C-x C-k c") 'kmacro-call-macro)

;; Customize the mark and kill ring
(setq-default global-mark-ring-max 5000) ; mark-ring contains is 5000 entries
(setq-default mark-ring-max 5000)  ; kill-ring contains is 5000 entries
(setq-default kill-ring-max 5000)  ; kill-ring capacity is 5000 entries

;; kill whole line and move the next line up
(setq-default kill-whole-line t)

;; Add a new line to end of file
(setq-default mode-require-final-newline t)

;; Automatically add parenthesis
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; =============================================================================
;; Buffer movements
;; =============================================================================
;; Moving the cursor by paragraph
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-P") 'backward-list)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-N") 'forward-list)

;; Jump from window to window
(use-package ace-window
  :ensure t :demand t
  :bind
  ("M-²" . ace-window))

;; Jump to chars and lines
(use-package ace-jump-mode
  :ensure t :demand t
  :bind
  ("C-c <" . ace-jump-char-mode)
  ("C-c SPC" . ace-jump-line-mode))

;; Move buffers when needed, this is not eavily used but still can be useful
(use-package buffer-move
  :commands (buf-move-up buf-move-down buf-move-left buf-move-right)
  :bind
  ("<C-S-up>" . buf-move-up)
  ("<C-S-down>" . 'buf-move-down)
  ("<C-S-left>" . buf-move-left)
  ("<C-S-right>" . buf-move-right))

;; Delete other window when typing C-x 1 and retreive the windows when typing it
;; again
(use-package zygospore
  :demand t :ensure t
  :bind
  ("C-x 1" . zygospore-toggle-delete-other-windows))

;; Remove whitespace on modified code
(use-package ws-butler
  :commands ws-butler-mode
  :hook
  (prog-mode . ws-butler-mode))

;; Move to first non whitespace character of the line
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; =============================================================================
;; Utilities
;; =============================================================================
;; Go to scratch buffer and create it if needed
(defvar scratch-buffer-welcome-text
  (concat
   ";; This buffer is for text that is not saved, and for Lisp evaluation.\n"
   ";; To create a file, visit it with <open> and enter text in its buffer.\n\n"
   )
  "The text displayed in the scratch buffer when created")
(defun maybe-create-scratch-buffer nil
  "Create a scratch buffer."
  (interactive)
  (let ((scratch-exists (get-buffer "*scratch*")))
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (lisp-interaction-mode)
    (unless scratch-exists
      (with-current-buffer "*scratch*" (insert scratch-buffer-welcome-text)))))

(global-set-key (kbd "C-x ,") 'maybe-create-scratch-buffer)

;; Go to configuration directory
(defun edit-config ()
  "Open the config directory in dired."
  (interactive)
  (dired "~/.emacs.d"))

;; =============================================================================
;; Diff and merge
;; =============================================================================
;; ediff both A and B to C
(defun ediff-copy-both-to-C ()
  "Take both ediff changes to C buffer."
  (interactive)
  (ediff-copy-diff
   ediff-current-difference nil 'C nil
   (concat
    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer)
    )))
(defun add-d-to-ediff-mode-map ()
  (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)
