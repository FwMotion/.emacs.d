(defvar rmg:state-directory
  (concat rmg:user-emacs-dir "state/")
  "Location to contain all files holding \"current state\" of emacs.")
(make-directory rmg:state-directory t)
(rmg-on-startup (add-hook 'kill-emacs-hook
                          (lambda ()
                            (ignore-errors
                              (make-directory rmg:state-directory t)))))

(set-language-environment 'utf-8)
(prefer-coding-system (if running-on-windows
                          'utf-8-dos
                        'utf-8))
;; Default coding (prefer-utf-8 if available; otherwise, utf-8)
(defconst rmg:preferred-coding-system (or (ignore-errors
                                            (coding-system-base 'prefer-utf-8))
                                          'utf-8))
;; Default to DOS-style line ends; this is most portable to other platforms
(defconst rmg:preferred-coding-system-eol
  (coding-system-change-eol-conversion rmg:preferred-coding-system
                                       (if running-on-windows
                                           'dos
                                         'unix)))
(set-default-coding-systems rmg:preferred-coding-system-eol)
(set-selection-coding-system rmg:preferred-coding-system-eol)
(setq locale-coding-system rmg:preferred-coding-system)

;; Backup
(make-directory (concat rmg:user-emacs-dir "backup/") t)
(setq backup-directory-alist `(("." . ,(concat rmg:user-emacs-dir "backup/")))
      backup-by-copying t   ; don't delink hardlinks
      version-control t     ; use version numbers on backups
      delete-old-versions t ; automatically delete excess backups
      kept-new-versions 20  ; number of newest versions to keep
      kept-old-versions 5   ; number of old to keep
      vc-make-backup-files t; make backups even when in git/svn/etc
      tramp-backup-directory-alist backup-directory-alist)

;; Auto-save
(make-directory (concat rmg:user-emacs-dir "auto-save/") t)
(make-directory (concat rmg:user-emacs-dir "auto-save-list/") t)
(setq auto-save-file-name-transforms
      `((".*" ,(concat rmg:user-emacs-dir "auto-save/") t))
      tramp-auto-save-directory (concat rmg:user-emacs-dir "auto-save/"))

;; Save pointer location
(when (rmg-try-require 'saveplace)
  (setq-default save-place t)
  (setq save-place-file (concat rmg:state-directory "places")))

;; Exec path from shell
(when (and (not running-on-windows) (rmg-try-require 'exec-path-from-shell))
  (exec-path-from-shell-initialize))

;; Transparently open compressed files
(auto-compression-mode t)

;; Auto-revert
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

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

;; Final new-line
(setq-default require-final-newline t)

;; Set a bunch of variables; starting with ido's save file
(setq ido-save-directory-list-file (concat rmg:state-directory "ido.last")

      ;; Smex save file
      smex-save-file (concat rmg:state-directory "smex-items")

      ;; Helm files
      helm-documentation-file (concat rmg:state-directory "helm-doc.org")
      helm-adaptive-history-file (concat rmg:state-directory "helm-history")

      ;; Recentf
      recentf-save-file (concat rmg:state-directory "recentf")
      recentf-max-saved-items 40
      recentf-max-menu-items recentf-max-saved-items

      ;; Org-mode stuff
      org-clock-persist-file (concat rmg:state-directory "org-clock-save.el")
      org-id-locations-file (concat rmg:state-directory "org-id-locations")

      ;; Remember
      remember-data-file (concat rmg:state-directory "remember.notes")

      ;; Server auth
      server-auth-dir (concat rmg:state-directory "server/")

      ;; Cookies
      url-cookie-file (concat rmg:state-directory "url-cookies")

      ;; Game scores
      tetris-score-file (concat rmg:state-directory "tetris-scores")

      ;; Abbreviations
      abbrev-file-name (concat rmg:state-directory "abbrev_defs")

      ;; Desktop
      revive:app-restore-path rmg:state-directory
      revive:desktop-base-file-name "SessionDesktop.el"

      ;; Projectile
      projectile-cache-file (concat rmg:state-directory "projectile.cache")
      projectile-known-projects-file (concat rmg:state-directory
                                             "projectile-bookmarks.eld")

      ;; Eshell
      eshell-directory-name (concat rmg:user-emacs-dir "eshell/")
      eshell-login-script (concat eshell-directory-name "login")
      eshell-rc-script (concat eshell-directory-name "profile"))

;; Auto-trim trailing spaces
(defcustom rmg-auto-update-whitespace t
  "When non-nil, automatically update whitespace (indent and trailing spaces)
when saving files."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (other :tag "Ask" prompt))
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
            (unless (or (not (get-buffer-window (current-buffer)))
                        (eq 'nil rmg-auto-update-whitespace))
              (when (or (eq 't rmg-auto-update-whitespace)
                        (yes-or-no-p
                         "Update indent and remove trailing whitespace? "))
                (rmg/update-whitespace)))))

(provide 'rmg-files)
