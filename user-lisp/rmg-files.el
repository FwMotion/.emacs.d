(defvar rmg:state-directory
  (concat user-emacs-directory "state/")
  "Location to contain all files holding \"current state\" of emacs.")
(make-directory rmg:state-directory t)

;; Default coding
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Backup
(make-directory (concat user-emacs-directory "backup/") t)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backup/")))
      backup-by-copying t   ; don't delink hardlinks
      version-control t     ; use version numbers on backups
      delete-old-versions t ; automatically delete excess backups
      kept-new-versions 20  ; number of newest versions to keep
      kept-old-versions 5   ; number of old to keep
      vc-make-backup-files t; make backups even when in git/svn/etc
      )
(setq tramp-backup-directory-alist backup-directory-alist)

;; Auto-save
(make-directory (concat user-emacs-directory "auto-save/") t)
(make-directory (concat user-emacs-directory "auto-save-list/") t)
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t))
      tramp-auto-save-directory (concat user-emacs-directory "auto-save/"))

;; Cookies
(setq url-cookie-file (concat rmg:state-directory "url-cookies"))

;; Game scores
(setq tetris-score-file (concat rmg:state-directory "tetris-scores"))

;; Save pointer location
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat rmg:state-directory "places"))

;; Eshell
(require 'eshell)
(setq eshell-directory-name (concat user-emacs-directory "eshell/"))
(setq eshell-login-script (concat eshell-directory-name "login")
      eshell-rc-script (concat eshell-directory-name "profile"))

;; Transparently open compressed files
(auto-compression-mode t)

(provide 'rmg-files)
