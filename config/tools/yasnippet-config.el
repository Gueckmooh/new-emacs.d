;; =============================================================================
;; Yasnippet configuration
;; =============================================================================
(provide 'yasnippet-config)

;; =============================================================================
;; Basic configuration
;; =============================================================================
(use-package yasnippet
  :defer t
  :ensure t
  :ensure yasnippet-snippets
  :commands yas-minor-mode
  :hook
  (prog-mode . yas-minor-mode)
  (nxml-mode . yas-minor-mode))
