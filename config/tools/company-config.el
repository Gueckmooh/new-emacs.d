;; =============================================================================
;; Company configuration
;; =============================================================================
(provide 'company-config)

;; =============================================================================
;; Basic configuration
;; =============================================================================
(use-package company
  :ensure t
  :commands (company-mode)
  :bind ("<backtab>" . company-complete-common)
  :hook (prog-mode . company-mode)
  :config
  (setq-default company-idle-delay              0
                company-minimum-prefix-length   3
                company-show-quick-access       t
                company-tooltip-limit           20
                ))
