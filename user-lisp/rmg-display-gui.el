(when (fboundp 'hl-line-toggle-when-idle)
  (rmg-add-frame-start-hooks
   (lambda ()
     (if (display-graphic-p)
         (hl-line-toggle-when-idle 1 nil)
       (hl-line-toggle-when-idle -1 nil))))
  (hl-line-when-idle-interval 2))

;; Set up frames
(rmg-add-frame-start-hooks
 (lambda ()
   ;; Title format
   (setq frame-title-format (concat "%b - "
                                    (downcase user-login-name)
                                    "@"
                                    (downcase system-name)))

   ;; No scrollbars, toolbars, or tabbars
   (when (fboundp 'scroll-bar-mode)
     (scroll-bar-mode -1))
   (when (fboundp 'tool-bar-mode)
     (tool-bar-mode -1))
   (when (fboundp 'tabbar-mode)
     (tabbar-mode -1))

   ;; Fringe only on the right
   (set-fringe-mode '(0 . 8))))

(defcustom rmg-preferred-theme-gui 'twilight-anti-bright
  "Preferred GUI theme"
  :type 'symbol
  :group 'rmg)
(when (load-theme rmg-preferred-theme-gui t t)
  (rmg-add-frame-start-hooks
   (lambda ()
     (when (display-graphic-p)
       (enable-theme rmg-preferred-theme-gui)))))

(defcustom rmg-preferred-font-height 90
  "Preferred font height in GUI frames. Unit is tenths of a point. For
example, standard screen DPIs will typically use 90 for 9pt font; higher DPI
screens (17inch at 1920x1200) will typically be 110 for 11pt."
  :type 'integer
  :group 'rmg)
(rmg-add-frame-start-hooks
 (lambda ()
   (when (display-graphic-p)
     (set-face-attribute 'default nil
                         :height rmg-preferred-font-height))))

(defun rmg-w32-toggle-maximize-frame ()
  "Maximize the current frame in MS Windows"
  (w32-send-sys-command 61488))
(defun rmg-x11-toggle-maximize-frame ()
  "Maximize the current frame in X11"
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
(defun rmg/toggle-maximize-frame ()
  "Maximize the current frame depending on windowing system"
  (interactive)
  (cl-case window-system
    (x (rmg-x11-toggle-maximize-frame))
    (w32 (rmg-w32-toggle-maximize-frame))))

(defcustom rmg-maximize-on-setup t
  "Auto maximize the frame when starting"
  :type 'boolean
  :group 'rmg)
(rmg-add-frame-start-hooks
 (lambda ()
   (when (and (display-graphic-p)
              rmg-maximize-on-setup)
     (rmg/toggle-maximize-frame))))

;; Move mouse away from point
(rmg-add-frame-start-hooks
 (lambda ()
   (when (display-mouse-p)
     (mouse-avoidance-mode 'exile))))

(provide 'rmg-display-gui)
