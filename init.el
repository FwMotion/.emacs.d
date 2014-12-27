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

;; Load custom lisp
(add-to-list 'load-path (concat user-emacs-directory "user-lisp/"))

;; Main initialization modularized as follows
(require 'rmg-environment)
(require 'rmg-packages)
(rmg-try-require 'rmg-files)
(rmg-try-require 'rmg-startup)
(rmg-try-require 'rmg-panels)
(rmg-try-require 'rmg-misc-functions)
(rmg-try-require 'rmg-keybindings)
(rmg-try-require 'rmg-display-global)
(rmg-try-require 'rmg-display-console)
(rmg-try-require 'rmg-display-gui)
(rmg-try-require 'rmg-codestyle-global)
(rmg-try-require 'rmg-codestyle-javascript)
(ignore-errors (require 'site-customizations))
