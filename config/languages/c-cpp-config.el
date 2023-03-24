;; =============================================================================
;; C and C++ configuration
;; =============================================================================
(provide 'c-cpp-config)

;; @todo find a way to evaluate all this lazilly

;; =============================================================================
;; Style configuration
;; =============================================================================
;; Interactively set the c style
;;;###autoload
(defun define-my-c-style ()
  "Declare my c style."
  (c-add-style "my-c-style"
             '((c-basic-offset . 4)
               (c-offsets-alist
                (access-label . /)
                (arglist-cont . 0)
                (arglist-intro . +)
                (block-close . 0)
                (case-label . 0)
                (class-close . 0)
                (defun-block-intro . +)
                (defun-close . 0)
                (inclass . +)
                (inline-close . 0)
                (innamespace . 0)
                (member-init-intro . +)
                (namespace-close . 0)
                (statement . 0)
                (statement-block-intro . +)
                (statement-case-intro . +)
                (statement-cont . +)
                (topmost-intro . 0)
                (annotation-top-cont . 0)
                (annotation-var-cont . +)
                (arglist-close . c-lineup-close-paren)
                (arglist-cont-nonempty . c-lineup-arglist)
                (block-open . 0)
                (brace-entry-open . 0)
                (brace-list-close . 0)
                (brace-list-entry . 0)
                (brace-list-intro . +)
                (brace-list-open . +)
                (c . c-lineup-C-comments)
                (catch-clause . 0)
                (class-open . 0)
                (comment-intro . c-lineup-comment)
                (composition-close . 0)
                (composition-open . 0)
                (cpp-define-intro c-lineup-cpp-define +)
                (cpp-macro . -1000)
                (cpp-macro-cont . +)
                (defun-open . 0)
                (do-while-closure . 0)
                (else-clause . 0)
                (extern-lang-close . 0)
                (extern-lang-open . 0)
                (friend . 0)
                (func-decl-cont . +)
                (incomposition . +)
                (inexpr-class . +)
                (inexpr-statement . +)
                (inextern-lang . +)
                (inher-cont . c-lineup-multi-inher)
                (inher-intro . +)
                (inlambda . 0)
                (inline-open . +)
                (inmodule . +)
                (knr-argdecl . 0)
                (knr-argdecl-intro . +)
                (label . *)
                (lambda-intro-cont . +)
                (member-init-cont . c-lineup-multi-inher)
                (module-close . 0)
                (module-open . 0)
                (namespace-open . 0)
                (objc-method-args-cont . c-lineup-ObjC-method-args)
                (objc-method-call-cont c-lineup-ObjC-method-call-colons
                                       c-lineup-ObjC-method-call +)
                (objc-method-intro . [0])
                (statement-case-open . 0)
                (stream-op . c-lineup-streamop)
                (string . -1000)
                (substatement . +)
                (substatement-label . 2)
                (substatement-open . 0)
                (template-args-cont c-lineup-template-args +)
                (topmost-intro-cont . c-lineup-topmost-intro-cont)))))

;;;###autoload
(defun set-my-c-style ()
  "Set my c style."
  (interactive)
  (define-my-c-style)
  (c-set-style "my-c-style"))

(add-hook 'c-mode-hook #'set-my-c-style)
(add-hook 'c++-mode-hook #'set-my-c-style)

;; =============================================================================
;; Language configuration
;; =============================================================================
;; Use c++ mode for .h .tpp and .inl files extensions
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

;; Use ccls as language server
;; @todo do I need the defer ?
(use-package ccls :defer t
  :config
  (setq-default ccls-sem-highlight-method 'overlay))

;; Format the file with clang format
(use-package clang-format+
  :defer t
  :hook
  (c-mode . clang-format+-mode)
  (c++-mode . clang-format+-mode))
