(defmacro rmg-on-frames (window-setup new-frames focus-in &rest body)
  "Add hook to run after initialization completes and frame parameters have
been set up. If NEW-FRAMES is non-nil, it will also add a hook to run for all
new frames. Additionally, if FOCUS-IN is non-nil, add a hook to run when focus
changes.

\(fn WINDOW-SETUP NEW-FRAMES FOCUS-IN &rest BODY)"
  `(progn
     (when ,window-setup
       (add-hook 'window-setup-hook
                 (lambda () ,@body)
                 t))
     (when ,new-frames
       (add-hook 'after-make-frame-functions
                 (lambda (rmg-on-frame-frame)
                   (with-selected-frame rmg-on-frame-frame
                     ,@body))
                 t))
     (when ,focus-in
       (add-hook 'focus-in-hook
                 (lambda () ,@body)
                 t))))

(defmacro rmg-on-startup (&rest body)
  "Add hook to run after emacs initialization completes."
  `(add-hook 'emacs-startup-hook
             (lambda () ,@body)
             t))

(defmacro rmg-on-package (package &body)
  ;; TODO
  )

(provide 'rmg-hooks)
