;; =============================================================================
;; Doxygen configuration
;; =============================================================================
(provide 'doxygen-config)

;; =============================================================================
;; Basic configuration
;; =============================================================================
(use-package highlight-doxygen
  :commands (highlight-doxygen-mode)
  :hook
  (souffle-mode . highlight-doxygen-mode)
  :config
  (set-face-background 'highlight-doxygen-comment nil))
