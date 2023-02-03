;; =============================================================================
;; Company configuration
;; =============================================================================
(provide 'company-config)

;; =============================================================================
;; Basic configuration
;; =============================================================================
(use-package company
  :ensure t
  :bind ("<backtab>" . company-complete-common)
  :config
  (setq company-idle-delay              0
        company-minimum-prefix-length   3
        company-show-quick-access       t
        company-tooltip-limit           20
        )
  (add-hook 'prog-mode-hook 'company-mode))
