;; =============================================================================
;; Prog configuration
;;
;; This file contains the common configuration related to programming for emacs
;; =============================================================================
(provide 'prog-config)

;; =============================================================================
;; Utilities
;; =============================================================================
;;;###autoload
(defun desperately-compile (&optional file)
  "Traveling up the path, find a Makefile and
`compile'. Optionaly takes a parameter FILE which is the file it
will look for in alternative to the good old Makefile."
  (interactive)
  (let ((file (cond (file file) (t "Makefile"))))
    (when (locate-dominating-file default-directory file)
      (with-temp-buffer
        (cd (locate-dominating-file default-directory file))
        (call-interactively #'compile)))))

(define-key prog-mode-map (kbd "C-c RET") 'recompile)
(define-key prog-mode-map (kbd "C-x RET RET") 'desperately-compile)

;; Setup gdb
(require 'gdb-mi)
;; Non-nil means display source file containing the main routine at startup
(setq-default gdb-show-main t)
;; Show many windows in GDB
(setq-default gdb-many-windows t)

(use-package highlight-indent-guides
  :commands highlight-indent-guides-mode
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq-default highlight-indent-guides-method 'character)
  (setq-default highlight-indent-guides-auto-enabled nil)
  (set-face-foreground 'highlight-indent-guides-character-face "#6c6979"))

(autoload #'ansi-color-apply-on-region "ansi-color" "Display ansi colors" t)

;;;###autoload
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
