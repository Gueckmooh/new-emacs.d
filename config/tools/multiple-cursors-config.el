;; =============================================================================
;; Multiple cursors configuration
;; =============================================================================
(provide 'multiple-cursors-config)

;; =============================================================================
;; Basic configuration
;; =============================================================================
(use-package multiple-cursors
  :commands (mc/mark-next-like-this mc/mark-previous-like-this mc/mark-all-like-this)
  :ensure t
  :bind
  ("C-M-n" . mc/mark-next-like-this)
  ("C-M-p" . mc/mark-previous-like-this)
  ("M-RET" . mc/mark-all-like-this)
  :init
  (with-eval-after-load 'nxml-mode
    (define-key nxml-mode-map (kbd "C-M-n") nil)
    (define-key nxml-mode-map (kbd "C-M-p") nil)))
