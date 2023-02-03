;; =============================================================================
;; Go configuration
;; =============================================================================
(provide 'golang-config)

;; =============================================================================
;; Basic configuration
;; =============================================================================
(use-package go-mode
  :demand t
  :commands go-mode
  :init
  ;; Compiling go
  (defcustom gobuild-command (purecopy "go build -v && go test -v && go vet")
    "Go build command."
    :type 'string
    :group 'compilation)

  (defun go-compile (command &optional comint)
    "An implementation of the `compile' command for golang."
    (interactive
     (list
      (let ((command (eval gobuild-command)))
        (if (or compilation-read-command current-prefix-arg)
	    (compilation-read-command command)
	  command))
      (consp current-prefix-arg)))
    (unless (equal command (eval gobuild-command))
      (setq gobuild-command command))
    (save-some-buffers (not compilation-ask-about-save)
                       compilation-save-buffers-predicate)
    (setq-default compilation-directory default-directory)
    (compilation-start command comint))

  :config
  (define-key go-mode-map (kbd "C-x RET RET") 'go-compile))

;; Format go on save
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(with-eval-after-load 'lsp-config
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(with-eval-after-load 'lsp-config
  (setq-default lsp-go-use-gofumpt t))

;; =============================================================================
;; Look and feel
;; =============================================================================
(defface go-template-arg-face
  '((t :foreground "#02c7c7"))
  "Face to display on {{...}}."
  :group 'font-lock-faces)

(font-lock-add-keywords
 'go-mode
 '(("{{\\([^}]*\\)}}" 1 'go-template-arg-face prepend)
   ("{{[^}]*\\<\\(range\\|end\\|if\\|else\\|with\\|template\\|block\\|break\\|continue\\)\\>[^}]*}}"
    1 'font-lock-keyword-face prepend)
   ("{{[^}]*\\(\\.\\<[^} ]*\\>\\)[^}]*}}" 1 'font-lock-constant-face prepend)
   ))
