;; =============================================================================
;; Prog configuration
;;
;; This file contains the common configuration related to programming for emacs
;; =============================================================================
(provide 'prog-config)

;; =============================================================================
;; Utilities
;; =============================================================================
(defun desperately-compile (&optional file)
  "Traveling up the path, find a Makefile and
`compile'. Optionaly takes a parameter FILE which is the file it
will look for in alternative to the good old Makefile."
  (interactive)
  (let ((file (cond (file file) (t "Makefile"))))
    (when (locate-dominating-file default-directory makefile)
      (with-temp-buffer
        (cd (locate-dominating-file default-directory makefile))
        (call-interactively #'compile)))))

(define-key prog-mode-map (kbd "C-c RET") 'compile)
(define-key prog-mode-map (kbd "C-x RET RET") 'desperately-compile)
