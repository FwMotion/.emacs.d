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
             '("melpa" . "http://melpa.org/packages/")
             t)

(setq package-user-dir (concat rmg:user-emacs-dir "/elpa"))

;; TODO(rgrimm): defcustom to select HTTP or HTTPS (default HTTPS), then act
;; on it. Also convert default URLs to HTTPS when requested.
;; Currently MELPA doesn't support HTTPS :-(

(package-initialize)

(when (and (rmg-try-require 'f)
           (rmg-try-require 'dash))
  (defun was-compiled-p (path)
    "Does the directory at PATH contain any .elc files?"
    (--any-p (f-ext? it "elc") (f-files path)))

  (defun ensure-packages-compiled ()
    "If any packages installed with package.el aren't compiled yet, compile them."
    (--each (f-directories package-user-dir)
      (unless (was-compiled-p it)
        (byte-recompile-directory it 0))))

  (ensure-packages-compiled))

(provide 'rmg-packages)
