;; =============================================================================
;; Enzo Brignon's emacs configuration.
;;
;; This is the entry point of my emacs configuration. It gets the bare minimum
;; emacs requires to get packages then requires a collection of configuration
;; files that are either general configuration, or language/mode related
;; configuration. I'll try to stay clean and fast as much as possible.
;; =============================================================================
;; Capture launching time
(defvar init-el-start-time (current-time) "Time when init.el was started")

(require 'package)

;; Configure package, to be able to install and manage packages
(let*
    ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                  (not (gnutls-available-p))))
     ;; Use https unless ssl is not available
     (proto (unless no-ssl "https" "http")))
  ;; Add melpa packages
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t))

;; Use straight as package manager, it is better than package.el and permits to
;; have github based packages
(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)

;; Configure load path
(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'load-path "~/.emacs.d/config/tools")
(add-to-list 'load-path "~/.emacs.d/config/languages")
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Configure custom file, that contains emacs generated lisp
(setq custom-file "~/.emacs.d/custom.el")

;; Move the backup files in ~/.local/emacs_backups
(setq backup-directory-alist '(("" . "~/.local/emacs_backups")))

;; =============================================================================
;; Require subconfigurations
;; =============================================================================
(defun timed-require (symbol)
  "Require configurations files and output the time it tooks"
    (let ((time-before-init (current-time)))
      (progn
        (require symbol)
        (message
         "*** loading %s.el took %.2fs"
         (symbol-name symbol)
         (float-time (time-subtract (current-time) time-before-init))))))

;; =============================================================================
;; General configuration of emacs.
;; =============================================================================
(message "** loading general configuration files")
(timed-require 'general-config)
(timed-require 'prog-config)
(timed-require 'font-lock-config)

;; =============================================================================
;; Tools configuration.
;; =============================================================================
(message "** loading tools configuration files")
(timed-require 'helm-config)
(timed-require 'magit-config)
(timed-require 'company-config)
(timed-require 'flycheck-config)
(timed-require 'lsp-config)
(timed-require 'god-mode-config)
(timed-require 'powerline-config)
(timed-require 'projectile-config)
(timed-require 'yasnippet-config)
(timed-require 'ediff-config)
;; (timed-require 'p4-config)
(timed-require 'chezmoi-config)

;; =============================================================================
;; Language configurations.
;; =============================================================================
(message "** loading language configuration files")
(timed-require 'c-cpp-config)
(timed-require 'golang-config)
(timed-require 'python-config)
(timed-require 'jai-config)
(timed-require 'text-formats-config)

;; Display total loading time
(message "* loading init.el took %.2fs"
         (float-time (time-subtract (current-time) init-el-start-time)))
