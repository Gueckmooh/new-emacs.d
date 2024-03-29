;; =============================================================================
;; LSP configuration
;; =============================================================================
(provide 'lsp-config)

;; =============================================================================
;; Basic configuration
;; =============================================================================
(use-package lsp-mode
  :demand t
  :commands (lsp lsp-deferred)
  :defer t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq-default lsp-keymap-prefix "C-c l")
  :hook
  ;; The languages for which I use lsp-mode
  (c++-mode . lsp-deferred)
  (c-mode . lsp-deferred)
  (go-mode . lsp-deferred)
  (python-mode . lsp-deferred)
  ;; if you want which-key integration
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (custom-set-faces
   '(lsp-face-highlight-textual
     ((t (:inherit ahs-face))))
   '(lsp-face-highlight-read
     ((t (:inherit ahs-definition-face)))))
  )

(use-package lsp-ui :commands lsp-ui-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions]
    #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references]
    #'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "C-c l i") #'lsp-ui-imenu)
  (setq-default lsp-ui-sideline-show-hover nil)
  (setq-default lsp-ui-doc-include-signature t)
  (setq-default lsp-ui-doc-show-with-cursor t)
  (setq-default lsp-ui-doc-show-with-mouse nil))

(use-package helm-lsp :commands helm-lsp-workspace-symbol
  :bind ([remap xref-find-apropos] . helm-lsp-workspace-symbol)
  )

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-minimum-prefix-length 1
      lsp-lens-enable nil               ;; makes emacs too slow
      lsp-lens-place-position 'end-of-line
      lsp-signature-auto-activate nil)

;; Load vscode projects without removing all the other projects
;;;###autoload
(defun lsp-load-vscode-workspace-no-remove (file)
  "Load vscode workspace from FILE."
  (interactive "fSelect file to import: ")

  (let ((dir (f-dirname file)))
    (->> file
         (json-read-file)
         (alist-get 'folders)
         (-map (-lambda ((&alist 'path))
                 (lsp-workspace-folders-add (expand-file-name path dir)))))))
