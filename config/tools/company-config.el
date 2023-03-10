;; =============================================================================
;; Company configuration
;; =============================================================================
(provide 'company-config)

;; =============================================================================
;; Basic configuration
;; =============================================================================
(use-package company
  :ensure t
  :commands (company-mode company-complete-common)
  :bind ("M-i" . company-complete-common)
  :hook
  (prog-mode . company-mode)
  ;; Use recentf mode when using company mode
  (prog-mode . recentf-mode)
  :config
  (setq-default company-idle-delay              0
                company-minimum-prefix-length   3
                company-show-quick-access       t
                company-tooltip-limit           20
                ))
