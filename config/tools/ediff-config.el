;; =============================================================================
;; ediff configuration
;; =============================================================================
(provide 'ediff-config)

;; =============================================================================
;; Basic configuration
;; =============================================================================
;;;###autoload
(defun command-line-ediff (switch)
  "Use Emacs as diff tool. SWITCH is unused."
  (let ((file-A (pop command-line-args-left))
        (file-B (pop command-line-args-left)))
    (ediff file-A file-B ())))

;;;###autoload
(defun command-line-ediff-merge (switch)
  "Use Emacs as merge tool. SWITCH is unused."
  (let ((file-A (pop command-line-args-left))
        (file-B (pop command-line-args-left))
        (merge-file (pop command-line-args-left)))
    (ediff-merge file-A file-B () merge-file)))

;;;###autoload
(defun command-line-ediff-merge-with-ancestor (switch)
  "Use Emacs as merge tool. SWITCH is unused."
  (let ((file-A (pop command-line-args-left))
        (file-B (pop command-line-args-left))
        (file-ancestor (pop command-line-args-left))
        (merge-file (pop command-line-args-left)))
    (ediff-merge-with-ancestor file-A file-B file-ancestor () merge-file)))


(add-to-list 'command-switch-alist '("-ediff" . command-line-ediff))
(add-to-list 'command-switch-alist '("-ediff-merge" . command-line-ediff-merge))
(add-to-list 'command-switch-alist
             '("-ediff-merge3" . command-line-ediff-merge-with-ancestor))
