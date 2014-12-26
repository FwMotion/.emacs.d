;; Make loaded files drop messages
(setq force-load-messages t)

;; ELPA set up
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade-repo.org/packages/")
	     t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/")
	     t)

;; TODO(rgrimm): defcustom to select HTTP or HTTPS (default HTTPS), then act
;; on it. Also convert default URLs to HTTPS when requested.
;; Currently MELPA doesn't support HTTPS :-(

(package-initialize t)

(provide 'rmg-packages)
