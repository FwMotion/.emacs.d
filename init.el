;; User info
(setq user-full-name "Robert Grimm"
      user-mail-address (rot13 "ebo@sjzbgvba.pbz"))

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
