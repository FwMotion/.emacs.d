(require 'dash)

(defun jail-windows-mode-is-active-p ()
  "Return t when jail-windows-mode is on; otherwise, nil"
  (cdr (assoc 'jail-windows-mode (frame-parameters))))

(define-minor-mode jail-windows-mode
  "Jail the windows to a set layout."
  ;;:lighter " Jail"
  :global t
  :variable ((jail-is-active-p) .
             (lambda (state)
               (modify-frame-parameters nil `((jail-windows-mode . ,state)))))
  (message "jail-windows-mode %sabled"
           (if (jail-windows-mode-is-active-p)
               "en"
             "dis")))

(provide 'jail-windows)
