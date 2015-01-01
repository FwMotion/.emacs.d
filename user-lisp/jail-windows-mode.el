(require 'dash)

(defvar jail-windows--window-options-by-frame '())
(defvar jail-windows--registered-layouts '())
(defconst jail-windows--default-window-options
  '((active-config . none)
    (groups . (;(("^\\*magit" "^\\*something else")
                       ; . (#<window 1>))
                       ))))


;;;###autoload
(defun jail-windows/register-layout (layout-name layout-def)
  (add-to-list 'jail-windows--registered-layouts
               (cons layout-name layout-def)))

;;;###autoload
(defun jail-windows/activate-layout (layout-name)
  ;; TODO
  )

(defun jail-windows--reverse-string-match-p (string regexp)
  "Call string-match-p with reversed argument order."
  (string-match-p string regexp))

(defun jail-windows--display-buffer-override (buffer alist)
  "Take over window placement... ALIST is ignored"
  (let ((buffer-name (buffer-name buffer))
        (window-options (cdr (assoc (window-frame)
                                    jail-windows--window-options-by-frame)))
        (available-windows (window-list (window-frame) 'nominibuf))
        (matched-group))
    ;; For each group defined in the jail-windows' window-options
    (-each-while (cdr (assoc 'groups window-options))
        ;; Stop iterating when a group has matched
        (lambda (item) (not matched-group))
      (lambda (group)
        ;; Pull the regexp list and the window list from the group
        (let ((regexp-list (car group))
              (window-list (cdr group))
              (-compare-fn jail-windows--reverse-string-match-p))
          ;; Check if this list's regexp matches the BUFFER argument's name
          (if (-contains? regexp-list buffer-name)
              ;; When it does match, only use windows from this group
              (progn
                (setq matched-group t)
                (setq available-windows window-list))
            ;; When it doesn't, remove this group's windows from the list
            (progn
              (setq available-windows
                    (-difference available-windows window-list)))))))

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

(defadvice display-buffer-other-frame (around jail-window-hook activate)
  "Handles jailing the window"
  (if (jail-windows/active-p)
      (progn (let ((display-buffer-overriding-action
                    '(jail-windows--display-buffer-override)))
               ad-do-it))
    (progn ad-do-it)))

;; TODO: (defadvice display-buffer-other-frame ...)

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
