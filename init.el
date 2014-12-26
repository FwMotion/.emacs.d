;; User info
(setq user-full-name "Robert Grimm"
      user-mail-address (rot13 "tevzz.ebo@tznvy.pbz"))

;; Early removal of GUI scrollbars and toolbar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Load custom lisp
(add-to-list 'load-path (concat user-emacs-directory "user-lisp/"))

;; Main initialization modularized as follows
(require 'rmg-environment)
(require 'rmg-packages)
(require 'rmg-files)
(require 'rmg-startup)
;;(require 'rmg-display-global)  -- global display stuff
;;(require 'rmg-display-console) -- hook to set up console display
;;(require 'rmg-display-gui)     -- hook to set up GUI display
(require 'rmg-panels)
(require 'rmg-misc-functions)
(require 'rmg-keybindings)
;;(require 'rmg-codestyle-global)
;;(require 'rmg-codestyle-javascript)

(ignore-errors (require 'site-customizations))

;; Move customizations to separate file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
