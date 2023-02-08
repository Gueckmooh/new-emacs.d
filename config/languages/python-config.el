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

(setq-default python-shell-interpreter "/usr/bin/python")
(setq-default python-indent-offset 4)

(with-eval-after-load 'lsp-config
  (setq-default lsp-pylsp-plugins-pydocstyle-enabled nil)
  (setq-default lsp-pylsp-plugins-pycodestyle-max-line-length 81)
  (setq-default lsp-pyls-plugins-flake8-max-line-length 81))
