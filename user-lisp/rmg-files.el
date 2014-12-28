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
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backup/")))
      backup-by-copying t   ; don't delink hardlinks
      version-control t     ; use version numbers on backups
      delete-old-versions t ; automatically delete excess backups
      kept-new-versions 20  ; number of newest versions to keep
      kept-old-versions 5   ; number of old to keep
      vc-make-backup-files t; make backups even when in git/svn/etc
      tramp-backup-directory-alist backup-directory-alist)

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

;; Abbreviations
(setq abbrev-file-name (concat rmg:state-directory "abbrev_defs"))

;; Eshell
(require 'eshell)
(setq eshell-directory-name (concat user-emacs-directory "eshell/"))
(setq eshell-login-script (concat eshell-directory-name "login")
      eshell-rc-script (concat eshell-directory-name "profile"))

;; Exec path from shell
(when (rmg-try-require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Transparently open compressed files
(auto-compression-mode t)

;; Auto-revert
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Fix a problem with auto-reverting buffers sometimes recentering
(defcustom rmg-message-on-revert-recenter nil
  "When non-nil, message each time a revert attempts to recenter the buffer."
  :type 'boolean
  :group 'rmg)
(defadvice recenter (around rmg-recenter-not-after-revert activate)
  "Avoid recentering after reverting a buffer."
  (if revert-buffer-in-progress-p
      (when rmg-message-on-revert-recenter
        (message "Skipping recenter after revert."))
    ad-do-it))

;; ido save file
(setq ido-save-directory-list-file (concat rmg:state-directory "ido.last"))

;; Final new-line
(setq-default require-final-newline t)

;; Smex save file
(setq smex-save-file (concat rmg:state-directory "smex-items"))

;; Helm files
(setq helm-documentation-file (concat rmg:state-directory "helm-doc.org")
      helm-adaptive-history-file (concat rmg:state-directory "helm-history"))

;; Recentf file
(setq recentf-save-file (concat rmg:state-directory "recentf"))

;; Auto-trim trailing spaces
(defcustom rmg-auto-update-whitespace t
  "When non-nil, automatically update whitespace (indent and trailing spaces)
when saving files."
  :type 'boolean
  :group 'rmg)

(defun rmg/update-whitespace (&optional start end)
  "Automatically update indent whitespace and remove trailing whitespace."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list nil nil))))
  (if indent-tabs-mode
      (tabify (or start (point-min)) (or end (point-max)))
    (untabify (or start (point-min)) (or end (point-max))))
  (delete-trailing-whitespace start end))

(add-hook 'before-save-hook
          (lambda ()
            (when (or rmg-auto-update-whitespace
                      (yes-or-no-p
                       "Update indent and remove trailing whitespace? "))
              (rmg/update-whitespace))))

(provide 'rmg-files)
