;; Make loaded files drop messages
(setq force-load-messages t)

(defvar rmg-missing-packages-list nil
  "List of packages that `try-require' can't find.")
(defun rmg-try-require (feature)
  "Attempt to load a library or module. Return true if the library given as
argument is successfully loaded. If not, instead of an error, just add the
package to a list of missing packages."
  (condition-case err
      ;; protected form
      (progn
        (message "Checking for library `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "Checking for library `%s'... Found" feature))
    ;; error handler
    (file-error
     (progn
       (message "Checking for libary `%s'... Missing" feature)
       (add-to-list 'rmg-missing-packages-list feature 'append))
     nil)))

;; ELPA set up
(rmg-try-require 'package)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/")
             t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             t)

(setq package-user-dir (concat rmg:user-emacs-dir "/elpa"))

;; TODO(rgrimm): defcustom to select HTTP or HTTPS (default HTTPS), then act
;; on it. Also convert default URLs to HTTPS when requested.

(package-initialize)

;; Initialize package contents
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install any missing packages from stable repos
(dolist (pkg rmg:packages)
  (when (not (package-installed-p pkg))
    (if (assoc pkg package-archive-contents)
        (package-install pkg)
      (push pkg rmg:packages-unstable))))

;; Install any missing packages from unstable repos
(when (rmg-try-require 'dash)
  (when (--any? (not (package-installed-p it)) rmg:packages-unstable)
    (let ((package-archives '(("melpa" . "https://melpa.org/packages/"))))
      (package-refresh-contents)
      (dolist (pkg rmg:packages-unstable)
        (when (and (not (package-installed-p pkg))
                   (assoc pkg package-archive-contents))
          (package-install pkg))))
    (package-refresh-contents)))

;; Compile any packages that came with source-controlled home
(when (and (rmg-try-require 'f)
           (rmg-try-require 'dash))
  (defun rmg--was-compiled-p (path)
    "Does the directory at PATH contain any .elc files?"
    (--any-p (f-ext? it "elc") (f-files path)))

  (defun rmg/ensure-packages-compiled ()
    "If any packages installed with package.el aren't compiled yet, compile
them."
    (interactive)
    (--each (f-directories package-user-dir)
      (unless (rmg--was-compiled-p it)
        (byte-recompile-directory it 0))))

  (rmg/ensure-packages-compiled))

;; List packages that are installed and not part of requested list
(when (rmg-try-require 'dash)
  (defun rmg/list-unaccounted-packages ()
    "Like `package-list-packages', but shows only the packages that are
installed and are not in `rmg:packages'. Useful for cleaning out unwanted
packages.

Inspired by https://stackoverflow.com/a/15363401"
    (interactive)
    (package-show-package-list
     (--filter (and (not (memq it rmg:packages))
                    (not (memq it rmg:packages-unstable))
                    (not (package-built-in-p it))
                    (package-installed-p it))
               (mapcar #'car package-archive-contents)))))

(provide 'rmg-packages)
