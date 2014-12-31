(require 'dash)

(defvar jail-windows--window-options-by-frame '())
(defconst jail-windows--default-window-options
  '((active-config . none)
    (group-regexps . (;(group-name . ("^\\*magit" "^\\*something"))
                      ))
    (group-windows . (;(group-name . (#<window 1> #<window 2>))
                      ))))


;;;###autoload
(defun jail-windows/register-layout (layout-name layout-def)
  ;; TODO
  )

;;;###autoload
(defun jail-windows/activate-layout (layout-name)
  ;; TODO
  )

(defun jail-windows--reverse-string-match-p (string regexp)
  (string-match-p string regexp))

(defun jail-windows--display-buffer-override (buffer alist)
  "Take over window placement... ALIST is ignored"
  (let ((window-options (cdr (assoc (window-frame)
                                    jail-windows--window-options-by-frame)))
        (available-windows (window-list (window-frame) 'nominibuf))
        (matched-group))
    ;; TODO
    (-each-while (cdr (assoc 'group-regexps window-options))
        (lambda (item) (not matched-group))
      (lambda (group-regexps)
        (let ((group-name (car group-regexps))
              (-compare-fn jail-windows--reverse-string-match-p))
          (if (-contains? (cdr group-regexps) (buffer-name buffer))
              (progn )
            (progn
              (setq available-windows
                    (-difference available-windows
                                 (cdr (assoc group-name
                                             (assoc 'group-windows
                                                    window-options))))))))))

    ;; Pick among the remaining windows
    (if (not available-windows)
        nil
      (progn ))))

(defun jail-windows--build-frame-defaults (&optional frame)
  (cons (or frame (window-frame))
        jail-windows--default-window-options))

(defadvice display-buffer (around jail-window-hook activate)
  "Handles jailing the window"
  (if (jail-windows/active-p)
      (progn (let ((display-buffer-overriding-action
                    '(jail-windows--display-buffer-override)))
               ad-do-it))
    (progn ad-do-it)))

;;;###autoload
(defun jail-windows/active-p (&optional frame)
  "Return t when jail-windows-mode is on; otherwise, nil"
  (cdr (assoc 'jail-windows-mode (frame-parameters frame))))

;;;###autoload
(define-minor-mode jail-windows-mode
  "Jail the windows to a set layout."
  :global t
  :variable ((jail-windows/active-p) .
             (lambda (state)
               (modify-frame-parameters nil `((jail-windows-mode . ,state)))))
  (when (jail-windows/active-p)
    (let ((window-options (assoc (window-frame)
                                 jail-windows--window-options-by-frame)))
      (if window-options
          (progn (jail-windows-activate-layout (assoc 'active-config
                                                      window-options)))
        (progn (push (jail-windows--build-frame-defaults)
                     jail-windows--window-options-by-frame)))))
  (message "jail-windows-mode %sabled"
           (if (jail-windows/active-p)
               "en"
             "dis")))

(provide 'jail-windows-mode)
