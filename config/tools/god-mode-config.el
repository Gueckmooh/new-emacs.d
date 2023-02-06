;; =============================================================================
;; God mode configuration
;; =============================================================================
(provide 'god-mode-config)

;; =============================================================================
;; Basic configuration
;; =============================================================================
(use-package god-mode
  :ensure t
  :demand t
  :init
  (require 'god-mode-isearch)

  (defun my/god-mode-update-cursor ()
    (setq cursor-type (if (or god-local-mode buffer-read-only)
                          'hollow
                        'box)))

  (defun my/replace-char (arg)
    (interactive "*p")
    (quoted-insert arg)
    (delete-char 1))

  (defun quoted-insert-or-quit ()
    (interactive)
    (if buffer-read-only
        (quit-window)
      (call-interactively 'quoted-insert))
    )

  :bind (("<escape>" . god-mode-all)
         ("M-m" . god-mode-all)
         :map god-local-mode-map
         ("X" . helm-M-x)
         ("œ" . ace-window)
         ("²" . ace-window)
         ("z" . repeat)
         ("&" . zygospore-toggle-delete-other-windows)
         ("é" . split-window-below)
         ("\"" . split-window-right)
         ("à" . delete-window)
         ("h" . delete-backward-char)
         ("i" . god-mode-all)
         ("<" . beginning-of-buffer)
         (">" . end-of-buffer)
         ("I" . (lambda () (interactive) (beginning-of-line)
                  (open-line 1) (god-mode-all)))
         ("A" . (lambda () (interactive) (end-of-line)
                  (god-mode-all)))
         ("O" . other-window)
         ("o" . (lambda () (interactive) (end-of-line)
                  (newline) (god-mode-all)))
         ("B" . helm-buffers-list)
         ("Q" . quit-window)
         ("V" . scroll-down-command)
         ("K" . (lambda () (interactive) (kill-buffer (window-buffer))))
         ("U" . undo-tree-visualize)
         ("$" . keyboard-quit)
         ("W" . kill-ring-save)
         ("F" . helm-find-files)
         ("R" . my/replace-char)
         ("q" . quoted-insert-or-quit)
         ("!" . next-error)
         (":" . my-evil-ex)
         :map isearch-mode-map
         ("<escape>" . god-mode-isearch-activate)
         ("M-m" . god-mode-isearch-activate)
         :map god-mode-isearch-map
         ("<escape>" . god-mode-isearch-disable)
         ("M-m" . god-mode-isearch-disable))
  :config
  (add-hook 'god-mode-enabled-hook #'my/god-mode-update-cursor)
  (add-hook 'god-mode-disabled-hook #'my/god-mode-update-cursor)

  (add-to-list 'god-exempt-major-modes 'eshell-mode)
  (add-to-list 'god-exempt-major-modes 'p4-basic-mode)
  (add-to-list 'god-exempt-major-modes 'p4-diff-mode)

  (define-key god-local-mode-map (kbd ";") 'comment-dwim)
  (define-key god-local-mode-map (kbd "<f1>") 'help-command)

  (add-hook 'compilation-mode-hook #'god-local-mode)
  (add-hook 'help-mode-hook #'god-local-mode)

  (defun god-mode-all-if-not () (if (not god-global-mode) (god-mode-all)))
  (defvar idle-god-mode-timer
    (run-with-idle-timer 10 t 'god-mode-all-if-not)
    "Timer that enables god mode after 10 seconds of inactivity")

  (defun god-activate-idle-timer (&optional delay)
    "Activates the idle timer with DELAY if set, 10 seconds otherwise."
    (interactive)
    (let ((delay (cond (delay delay) (t 10))))
      (if (not idle-god-mode-timer)
          (setq idle-god-mode-timer
                (run-with-idle-timer delay t 'god-mode-all-if-not)))))

  (defun god-deactivate-idle-timer ()
    "Deactivates the idle timer."
    (interactive)
    (if idle-god-mode-timer
        (progn (cancel-timer idle-god-mode-timer)
               (setq idle-god-mode-timer nil))
      ))
  (god-deactivate-idle-timer)

  )

;; Activate god-mode by default at startup
(if (package-installed-p 'god-mode) (god-mode))
