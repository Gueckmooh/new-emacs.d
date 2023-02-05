;; =============================================================================
;; Powerline configuration
;; =============================================================================
(provide 'powerline-config)

;; @todo change this package so that it can be used with/without god mode and
;; anzu

;; =============================================================================
;; Basic configuration
;; =============================================================================
(use-package powerline
  :ensure t
  :demand t
  :config
  ;; Some faces definitions
  (defface mypowerline-active2
    '((t (:inherit mode-line)))
    "Powerline face 3."
    :group 'powerline)

  (defface mypowerline-god-active
    '((t (:background "dark cyan" :foreground "white" :inherit mode-line)))
    "Powerline face 1."
    :group 'powerline)

  (defface mypowerline-god-inactive
    '((t (:background "dark red" :foreground "white" :inherit mode-line)))
    "Powerline face 1."
    :group 'powerline)

  (defface mypowerline-god-other
    '((t (:background "dark slate gray" :foreground "white"
                      :inherit mode-line)))
    "Powerline face 1."
    :group 'powerline)

  (defface mypowerline-anzu-active
    '((t (:background "dark magenta" :foreground "white" :inherit mode-line)))
    "Powerline face 1."
    :group 'powerline)

  (defface mypowerline-buffedit
    '((t (:inherit mode-line :extend t :background "white"
                   :foreground "black")))
    "Powerline face on buffer name when edited."
    :group 'powerline)

  ;; Define what to display for the vim like status
  (defvar powerline-god-status-other-modes
    '(dired-mode magit-status-mode Man-mode
                                       magit-diff-mode debugger-mode
                                       apropos-mode package-menu-mode)
    "The list of modes that are displayed with OTHER rather than GOD or INSERT")
  (defpowerline powerline-god
    (let ((god-str
           (cond
            (god-local-mode "GOD     ")
            ((member major-mode powerline-god-status-other-modes) "OTHER   ")
            (t "INSERT  "))))
      god-str))

  ;; Use anzu in the powerline
  (with-eval-after-load 'anzu
    ;; Do not display the vanilla anzu mode line
  (setq-default anzu-cons-mode-line-p nil))
  (defpowerline powerline-anzu
    (let ((god-str (anzu--update-mode-line)))
      god-str))

  (defun my/powerline-buffer-id (&optional face pad)
    (powerline-raw
     (format-mode-line
      (propertize
       (format-mode-line mode-line-buffer-identification)
       'face face
       'mouse-face 'mode-line-highlight
       'help-echo
       "Buffer name\n\ mouse-1: Previous buffer\n\ mouse-3: Next buffer"
       'local-map
       (let ((map (make-sparse-keymap)))
         (define-key map [mode-line mouse-1] 'mode-line-previous-buffer)
         (define-key map [mode-line mouse-3] 'mode-line-next-buffer)
         map)))
     face pad))


  ;; ===========================================================================
  ;; Power line configuration
  ;; ===========================================================================
  (defun powerline-custom-theme ()
    "Setup the default mode-line."
    (interactive)
    (setq-default
     mode-line-format
     '((:eval
        (let* ((active (powerline-selected-window-active))
               (mode-line-buffer-id (if active 'mode-line-buffer-id
                                      'mode-line-buffer-id-inactive))
               (mode-line (if active 'mode-line 'mode-line-inactive))
               (face0 (if active 'powerline-active0 'powerline-inactive0))
               (face1 (if active 'powerline-active1 'powerline-inactive1))
               (face2 (if active 'mypowerline-active2 'powerline-inactive2))
               (face-buf (if active
                             (if (buffer-modified-p) 'mypowerline-buffedit
                               'powerline-active0)
                           (if (buffer-modified-p) 'powerline-inactive2
                             'powerline-inactive0)))
               (face-god
                (if active
                    (cond (god-local-mode 'mypowerline-god-active)
                          ((member major-mode
                                   '(dired-mode magit-status-mode Man-mode
                                                magit-diff-mode debugger-mode
                                                apropos-mode package-menu-mode)
                                   ) 'mypowerline-god-other)
                          (t 'mypowerline-god-inactive))
                  'powerline-inactive1)
                )
               (face-anzu (if anzu--state 'mypowerline-anzu-active
                            (if active 'powerline-active0 'powerline-inactive0)))
               (separator-left (intern
                                (format "powerline-%s-%s"
                                        (powerline-current-separator)
                                        (car powerline-default-separator-dir))))
               (separator-right (intern
                                 (format "powerline-%s-%s"
                                         (powerline-current-separator)
                                         (cdr powerline-default-separator-dir))))
               (lhs (list
                     (powerline-god face-god 'l)
                     (funcall separator-left face-god face-anzu)
                     (when anzu--state (powerline-anzu face-anzu 'l))
                     (when anzu--state (powerline-raw " " face-anzu))
                     (when anzu--state (funcall separator-left face-anzu face0))
                     (powerline-raw "%*" face0 'l)
                     (when powerline-display-buffer-size
                       (powerline-buffer-size face0 'l))
                     (when powerline-display-mule-info
                       (powerline-raw mode-line-mule-info face0 'l))
                     (funcall separator-left face0 face-buf)
                     (my/powerline-buffer-id
                      `(,face-buf ,mode-line-buffer-id) 'l)
                     (powerline-raw " " face-buf)
                     (funcall separator-left face-buf face1)
                     (when (and (boundp 'erc-track-minor-mode)
                                erc-track-minor-mode)
                       (powerline-raw erc-modified-channels-object face1 'l))
                     (powerline-major-mode face1 'l)
                     (powerline-process face1)
                     ;; (powerline-minor-modes face1 'l)
                     (powerline-narrow face1 'l)
                     (powerline-raw " " face1)
                     (funcall separator-left face1 face2)
                     (powerline-vc face2 'r)))
               (rhs (list (powerline-raw global-mode-string face2 'r)
                          (funcall separator-right face2 face1)
                          (unless window-system
                            (powerline-raw (char-to-string #xe0a1) face1 'l))
                          (powerline-raw "%4l" face1 'l)
                          (powerline-raw ":" face1 'l)
                          (powerline-raw "%3c" face1 'r)
                          (funcall separator-right face1 face0)
                          (powerline-raw " " face0)
                          (powerline-raw "%6p" face0 'r)
                          (when powerline-display-hud
                            (powerline-hud face0 face2))
                          (powerline-fill face0 0)
                          )))
          (concat (powerline-render lhs)
                  (powerline-fill face2 (powerline-width rhs))
                  (powerline-render rhs)))))))

  ;; Use the custom theme for the powerline
  (powerline-custom-theme)

  ;; Use arrow as spearator
  (setq-default powerline-default-separator 'arrow)
 )
