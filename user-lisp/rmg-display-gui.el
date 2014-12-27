
(when (display-graphic-p)
  (defcustom rmg-preferred-theme-gui 'twilight
    "Preferred GUI theme"
    :type 'symbol
    :group 'rmg)

  (when (load-theme rmg-preferred-theme-gui t t)
    (add-hook 'window-setup-hook
	      (lambda ()
		(enable-theme rmg-preferred-theme-gui))
	      t))

  (if running-on-windows
      (defun rmg-toggle-maximize-frame ()
	"Maximize the current frame in MS Windows"
	(interactive)
	(w32-send-sys-command 61488))
    (defun rmg-toggle-maximize-frame ()
      "Maximize the current frame in X11"
      (interactive)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			     '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			     '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))))

  (defcustom rmg-maximize-on-setup t
    "Auto maximize the frame when starting"
    :type 'boolean
    :group 'rmg)
  (when rmg-maximize-on-setup
    (add-hook 'window-setup-hook #'rmg-toggle-maximize-frame t)))

(provide 'rmg-display-gui)
