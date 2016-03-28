(when (fboundp 'hl-line-toggle-when-idle)
  (hl-line-when-idle-interval 2)
  (rmg-on-frames t nil nil
                 (hl-line-toggle-when-idle 1 nil)))

;; Set up frames
(rmg-on-frames t nil nil
               ;; Title format
               (setq frame-title-format (concat "%b - "
                                                (downcase user-login-name)
                                                "@"
                                                (downcase system-name)))

               ;; No menubars, scrollbars, toolbars, or tabbars
               (when (fboundp 'menu-bar-mode)
                 (menu-bar-mode -1))
               (when (fboundp 'scroll-bar-mode)
                 (scroll-bar-mode -1))
               (when (fboundp 'tool-bar-mode)
                 (tool-bar-mode -1))
               (when (fboundp 'tabbar-mode)
                 (tabbar-mode -1))

               ;; Fringe only on the right
               (set-fringe-mode '(0 . 8)))

(defcustom rmg-preferred-font-height 90
  "Preferred font height in GUI frames. Unit is tenths of a point. For
example, standard screen DPIs will typically use 90 for 9pt font; higher DPI
screens (17inch at 1920x1200) will typically be 110 for 11pt."
  :type '(choice (integer :tag "Custom")
                 (const :tag "9pt" 90)
                 (const :tag "11pt" 110))
  :group 'rmg
  :set (lambda (_var val)
         (set-default _var val)
         (set-face-attribute 'default
                             nil
                             :height val)))
(rmg-on-frames t t nil
               (when (display-multi-font-p)
                 (set-face-attribute 'default nil
                                     :height rmg-preferred-font-height)))

(defun rmg-w32-maximize-frame ()
  "Maximize the current frame in MS Windows"
  (w32-send-sys-command 61488))
(defun rmg-w32-restore-frame ()
  "Restore the current frame in MS Windows"
  (w32-send-sys-command 61728))
(defun rmg-x11-maximize-frame ()
  "Maximize the current frame in X11"
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(1 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(1 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
(defun rmg-x11-restore-frame ()
  "Restore the current frame in X11"
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(0 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(0 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
(defun rmg-x11-toggle-maximize-frame ()
  "Toggle Maximize/restore the current frame in X11"
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
(defun rmg/maximize-frame ()
  "Maximize the current frame depending on windowing system"
  (interactive)
  (cl-case window-system
    (x (rmg-x11-maximize-frame))
    (w32 (rmg-w32-maximize-frame))))
(defun rmg/toggle-maximize-frame ()
  "Toggle frame maximization"
  (interactive)
  (cl-case window-system
    (x (rmg-x11-toggle-maximize-frame))))

(defcustom rmg-maximize-on-setup t
  "Auto maximize the frame when starting"
  :type 'boolean
  :group 'rmg)
(rmg-on-frames t nil nil
               (when (and (display-graphic-p)
                          rmg-maximize-on-setup)
                 (rmg/maximize-frame)))

;; Move mouse away from point
(rmg-on-frames t t nil
               (when (display-mouse-p)
                 (mouse-avoidance-mode 'exile)))

(provide 'rmg-display-gui)
