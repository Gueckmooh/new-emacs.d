;; =============================================================================
;; Jai configuration
;; =============================================================================
(provide 'jai-config)

;; =============================================================================
;; Basic configuration
;; =============================================================================
(use-package jai-mode
  :straight (jai-mode
             :type git
             :host github
             :repo "krig/jai-mode"))