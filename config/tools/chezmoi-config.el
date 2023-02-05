;; =============================================================================
;; Chezmoi configuration
;; =============================================================================
(provide 'chezmoi-config)

;; =============================================================================
;; Basic configuration
;; =============================================================================
(use-package chezmoi
  :bind
  (("C-c C f" . chezmoi-find)
   ("C-c C s" . chezmoi-write))
  :config
;;;###autoload
  (defun load-chezmoi-company-backend ()
    (if chezmoi-mode
        (add-to-list 'company-backends 'chezmoi-company-backend)
      (delete 'chezmoi-company-backend 'company-backends)))
  (with-eval-after-load
      (progn
        (require 'chezmoi-company)
        (add-hook 'chezmoi-mode-hook #'load-chezmoi-company-backend))))
