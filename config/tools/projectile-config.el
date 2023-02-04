;; =============================================================================
;; Projectile configuration
;; =============================================================================
(provide 'projectile-config)

;; =============================================================================
;; Basic configuration
;; =============================================================================
(use-package projectile
  :hook (prog-mode . projectile-mode))

;; Use projectile with helm if possible
(with-eval-after-load 'helm-config
  (use-package helm-projectile
    :hook (projectile-mode . helm-projectile-on)
    :bind
    (:map projectile-mode-map
     ("C-c p" . projectile-command-map))
  ))
