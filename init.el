;; User info
(setq user-full-name "Robert Grimm"
      user-mail-address (rot13 "tevzz.ebo@tznvy.pbz"))

;; Early removal of GUI scrollbars and toolbar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Move customizations to separate file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(defgroup rmg nil
  "Customizations specific to my setup."
  :prefix "rmg-")

;; Load custom lisp
(add-to-list 'load-path (concat user-emacs-directory "user-lisp/"))

;; Main initialization modularized as follows
(require 'rmg-environment)
(require 'rmg-packages)
(rmg-try-require 'rmg-files)
(rmg-try-require 'rmg-startup)
(rmg-try-require 'rmg-panels)
(rmg-try-require 'rmg-generic-behavior)
(rmg-try-require 'rmg-misc-functions)
(rmg-try-require 'rmg-display-global)
(rmg-try-require 'rmg-display-console)
(rmg-try-require 'rmg-display-gui)
(rmg-try-require 'rmg-codestyle-global)
(rmg-try-require 'rmg-codestyle-c-like)
(rmg-try-require 'rmg-codestyle-java)
(rmg-try-require 'rmg-codestyle-javascript)
(rmg-try-require 'rmg-training-wheels)
(rmg-try-require 'rmg-keybindings)
(ignore-errors (require 'site-customizations))

;; TODOs:
;; - org mode
;; - ffir as replacement for ffip
;; - flycheck
;; - JS2-mode
;; - key for jquery-doc -- maybe C-h C-j? Only in JS modes?
;; - Java style

(add-hook 'after-init-hook
          (lambda ()
            (message "Missing packages: %s"
                     rmg-missing-packages-list))
          t)
