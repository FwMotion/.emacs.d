(setq inhibit-startup-screen t)

;; Pretend the variable is customized, to really bypass the echo. I know I'm
;; using GNU Emacs, even when I don't specifically customize it every place I
;; use my configuration.
(when running-gnu-emacs
  (setq inhibit-startup-echo-area-message (user-login-name))
  (put 'inhibit-startup-echo-area-message
       'saved-value (list (user-login-name))))

(server-start)

(provide 'rmg-startup)
