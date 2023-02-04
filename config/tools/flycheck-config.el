;; =============================================================================
;; Flycheck configuration
;; =============================================================================
(provide 'flycheck-config)

;; =============================================================================
;; Basic configuration
;; =============================================================================
(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :hook
  (emacs-lisp-mode . flycheck-mode)
  (lisp-interaction-mode . flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
