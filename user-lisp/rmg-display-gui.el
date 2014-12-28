(when (fboundp 'hl-line-toggle-when-idle)
  (add-hook 'window-setup-hook
            (lambda ()
              (if (display-graphic-p)
                  (hl-line-toggle-when-idle 1 nil)
                (hl-line-toggle-when-idle -1 nil)))
            t)
  (hl-line-when-idle-interval 2))

;; Set up the frame
(add-hook 'window-setup-hook
          (lambda ()
            (when (display-graphic-p)
              ;; Title format
              (setq frame-title-format (concat "%b - "
                                               (downcase user-login-name)
                                               "@"
                                               (downcase system-name)))

              ;; No scrollbars or toolbars
              (when (fboundp 'scroll-bar-mode)
                (scroll-bar-mode -1))
              (when (fboundp 'tool-bar-mode)
                (tool-bar-mode -1))

              ;; Fringe only on the right
              (set-fringe-mode '(0 . 8))))
          t)

(defcustom rmg-preferred-theme-gui 'twilight-anti-bright
  "Preferred GUI theme"
  :type 'symbol
  :group 'rmg)
(when (load-theme rmg-preferred-theme-gui t t)
  (add-hook 'window-setup-hook
            (lambda ()
              (when (display-graphic-p)
                (enable-theme rmg-preferred-theme-gui)))
            t))

(defcustom rmg-preferred-font-height 90
  "Preferred font height in GUI frames. Unit is tenths of a point. For
example, standard screen DPIs will typically use 90 for 9pt font; higher DPI
screens (17inch at 1920x1200) will typically be 110 for 11pt."
  :type 'integer
  :group 'rmg)
(add-hook 'window-setup-hook
          (lambda ()
            (when (display-graphic-p)
              (set-face-attribute 'default nil
                                  :height rmg-preferred-font-height)))
          t)

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
(add-hook 'window-setup-hook
          (lambda ()
            (when (and (display-graphic-p) rmg-maximize-on-setup)
              (rmg/toggle-maximize-frame)))
          t)

;; Move mouse away from point
(add-hook 'window-setup-hook
          (lambda ()
            (when (display-mouse-p)
              (mouse-avoidance-mode 'exile))))

(provide 'rmg-display-gui)
