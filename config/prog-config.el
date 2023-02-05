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

(define-key prog-mode-map (kbd "C-c RET") 'compile)
(define-key prog-mode-map (kbd "C-x RET RET") 'desperately-compile)

;; Setup gdb
(require 'gdb-mi)
;; Non-nil means display source file containing the main routine at startup
(setq-default gdb-show-main t)
;; Show many windows in GDB
(setq-default gdb-many-windows t)
