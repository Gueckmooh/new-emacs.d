;; =============================================================================
;; Font lock configuration
;;
;; This file configures the custom font lock used in my various files.
;; =============================================================================
(provide 'font-lock-config)

;; =============================================================================
;; Faces definition
;; =============================================================================
(defface my-custom-ok-face
  '((t :foreground "#22aa22"))
  "Face to display on @ok"
  :group 'font-lock-faces)

(defface my-custom-todo-face
  '((t :foreground "#b50303"))
  "Face to display on @todo"
  :group 'font-lock-faces)

(defface my-custom-easy-face
  '((t :inherit 'my-custom-ok-face))
  "Face to display on @easy"
  :group 'font-lock-faces)

(defface my-custom-question-face
  '((t :foreground "#e26302"))
  "Face to display on @question"
  :group 'font-lock-faces)

(defface my-custom-medium-face
  '((t :inherit 'my-custom-question-face))
  "Face to display on @medium"
  :group 'font-lock-faces)

(defface my-custom-hard-face
  '((t :inherit 'my-custom-todo-face))
  "Face to display on @hard"
  :group 'font-lock-faces)

(defface my-custom-progress-face
  '((t :foreground "#053efc"))
  "Face to display on @inprogress"
  :group 'font-lock-faces)

;; =============================================================================
;; Upgrade font lock function
;; =============================================================================
(defun upgrade-font-lock ()
  "Upgrades the font lock with @todo etc..."
  (font-lock-add-keywords
   nil
   '(("@\\<\\(todo\\|fixme\\|X+\\)\\>" 1 'my-custom-todo-face prepend)
     ("@\\<\\(ok\\|done\\|fixed\\)\\>" 1 'my-custom-ok-face prepend)
     ("@\\(\\?+\\|dropped\\|drop\\)" 1 'my-custom-question-face prepend)
     ("@\\(\\?+\\|question\\)" 1 'my-custom-question-face prepend)
     ("@\\<\\(inprogress\\|review\\|note\\)\\>" 1 'my-custom-progress-face prepend)
     ("@\\<\\(easy\\)\\>" 1 'my-custom-easy-face prepend)
     ("@\\<\\(medium\\)\\>" 1 'my-custom-medium-face prepend)
     ("@\\<\\(hard\\)\\>" 1 'my-custom-hard-face prepend)
     ))
  )

(add-hook 'prog-mode-hook #'upgrade-font-lock)
