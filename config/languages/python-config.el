;; =============================================================================
;; Python configuration
;; =============================================================================
(provide 'python-config)

;; =============================================================================
;; Basic configuration
;; =============================================================================
;; Use pipenv from within emacs
(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))
