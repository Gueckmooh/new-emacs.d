;; =============================================================================
;; Perforce configuration
;; =============================================================================
(provide 'p4-config)

;; =============================================================================
;; Basic configuration
;; =============================================================================

(use-package p4
  :ensure t
  :commands
  (p4-add
   p4-revert
   p4-edit
   p4-diff
   p4-ediff
   p4-change
   p4-opened))

;;;###autoload
(defun command-line-p4merge (switch)
  "Use emacs as p4 merge utility for perforce. SWITCH is unused."
  (let ((base (pop command-line-args-left))
        (sccs (pop command-line-args-left))
        (mine (pop command-line-args-left))
        (merg (pop command-line-args-left)))
    (ediff-merge-with-ancestor sccs mine base () merg)))
(add-to-list 'command-switch-alist '("-p4merge" . command-line-p4merge))
