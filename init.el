;; User info
(setq user-full-name "Robert Grimm"
      user-mail-address (rot13 "tevzz.ebo@tznvy.pbz"))

;; Early removal of GUI scrollbars and toolbar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Fix package loading on Aquamacs
(defvar rmg:user-emacs-dir
  (file-name-directory (or user-init-file load-file-name))
  "Real `user-emacs-directory' based on init-file location.")

(defgroup rmg nil
  "Customizations specific to my setup."
  :prefix "rmg-")

;; Load custom lisp
(add-to-list 'load-path (concat rmg:user-emacs-dir "user-lisp/"))

;; Standard package installs
(defconst rmg:packages '(anzu
                          dash
                          delight
                          docker
                          dockerfile-mode
                          editorconfig
                          exec-path-from-shell
                          f
                          flycheck
                          google-c-style
                          guide-key
                          helm
                          helm-projectile
                          hl-line+
                          jquery-doc
                          js2-mode
                          markdown-mode
                          magit
                          org
                          projectile
                          s
                          smex
                          undo-tree
                          yaml-mode

                          ;; For jail-windows-mode
                          ;; TODO(rgrimm): Move these out after
                          ;; jail-windows-mode is its own package
                          dash-functional
                          let-alist

                          ;; Themes
                          moe-theme
                          twilight-anti-bright-theme
                          twilight-theme)
  "Standard packages that should be automatically installed from stable repo")

(defvar rmg:packages-unstable '(powerline)
  "List of packages not available in stable repos or that should prefer unstable.")

;; Determine running environment and set up package loading
(require 'rmg-environment)
(require 'rmg-hooks)
(require 'rmg-packages)

;; Move customizations to separate file
(setq custom-file (concat rmg:user-emacs-dir "custom.el"))
(load custom-file 'noerror)

;; Before most of the regular custom inits
(when running-aquamacs
  (rmg-try-require 'rmg-aquamacs))

;; Run host-specific configuration
(rmg-try-require (intern (concat "rmg-host-" (downcase (system-name)))))

(require 'jail-windows-mode)

;; Run the rest of custom initialization
(rmg-try-require 'rmg-files)
(rmg-try-require 'rmg-startup)
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
;; - clean up and release jail-windows

(rmg-on-startup (message "Missing packages: %s"
                         rmg-missing-packages-list))
