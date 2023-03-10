
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
  (p4-opened
   p4-add
   p4-revert
   p4-edit
   p4-diff
   p4-ediff
   p4-change)
  :bind
  ("C-x p o" . p4-opened)
  ("C-x p a" . p4-add)
  ("C-x p r" . p4-revert)
  ("C-x p e" . p4-edit)
  ("C-x p =" . p4-diff))

;;;###autoload
(defun command-line-p4diff (switch)
  "Use emacs as a p4 diff utility. SWITCH is unused."
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

;;;###autoload
(defun command-line-p4merge (switch)
  "Use emacs as p4 merge utility. SWITCH is unused."
  (let ((base (pop command-line-args-left))
        (sccs (pop command-line-args-left))
        (mine (pop command-line-args-left))
        (merg (pop command-line-args-left)))
    (ediff-merge-with-ancestor sccs mine base () merg)))

(add-to-list 'command-switch-alist '("-p4diff" . command-line-p4diff))
(add-to-list 'command-switch-alist '("-p4merge" . command-line-p4merge))
