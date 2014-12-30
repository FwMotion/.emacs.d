;; Determine platform
(defconst running-on-windows
  (string-match "windows" (prin1-to-string system-type))
  "Whether emacs is currently running on MS Windows")
(defconst running-on-osx
  (string-match "darwin" (prin1-to-string system-type))
  "Whether emacs is currently running on Mac OSX")
(defconst running-on-linux
  (string-match "linux" (prin1-to-string system-type))
  "Whether emacs is currently running on GNU/Linux")

;; Determine variant of emacs
(defconst running-gnu-emacs
  (string-match "GNU Emacs" (version))
  "Whether the currently running version of emacs is GNU Emacs")
(defconst running-aquamacs
  (string-match "Aquamacs" (version))
  "Whether the currently running version of emacs is Aquamacs")

(provide 'rmg-environment)
