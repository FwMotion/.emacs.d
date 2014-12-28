;;; Inspired by http://fullofsta.rs/2012/01/an-improved-emacs-window-setup/

;;; Definition
;; Setup custom framework
(defun rmg/setup-windows-1 ()
  (interactive)
  (switch-to-buffer "*scratch*")
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
  (other-window 2)
  (split-window)
  (other-window -2)
  (enlarge-window-horizontally (- 80 (window-width)))
  (other-window 1)
  (enlarge-window-horizontally (- 80 (window-width)))
  (other-window -1)
  (setq rmg-temporary-window-top (nth 2 (window-list)))
  (setq rmg-temporary-window-bot (nth 3 (window-list))))

(defun rmg/setup-windows-2 ()
  (interactive)
  (switch-to-buffer "*scratch*")
  (delete-other-windows)
  (split-window-horizontally)
  (balance-windows)
  (other-window 1)
  (split-window)
  (other-window -1)
  (enlarge-window-horizontally (- 80 (window-width)))
  (setq rmg-temporary-window-top (nth 1 (window-list)))
  (setq rmg-temporary-window-bot (nth 2 (window-list))))

(setq rmg-temporary-window-top 'nil)
(setq rmg-temporary-window-bot 'nil)

;; Switch to the buffer in a special window
(defun rmg-special-display (buffer window)
  (if (window-live-p window)
      (with-selected-window window
        (switch-to-buffer buffer)
        window)
    nil))

;; Match buffers to the top window
(dolist (buffer-regexp-top '("^\\*Apropros\\*$"
                             "^\\*Backtrace\\*$"
                             "^\\*Colors\\*$"
                             "^\\*Compile-Log\\*$"
                             "^\\*Help\\*$"
                             "^\\*Locate\\*$"
                             "^\\*Pp Eval Output\\*$"
                             "^\\*ediff.*\\*$"
                             ;; shell and eshell
                             "^\\*e?shell"
                             ;; js comint
                             "^\\*js\\*$"
                             "^\\*info\\*$"
                             "^\\*local variables\\*$"
                             "^\\*magit-diff"))
  (add-to-list 'display-buffer-alist
               (cons buffer-regexp-top
                     (cons #'(lambda (buffer &optional data)
                               (rmg-special-display buffer
                                                    rmg-temporary-window-top))
                           nil))
               t))

;; Match buffers to the bottom window
(dolist (buffer-regexp-bot '("^\\*Completions\\*$"
                             "^\\*Ido Completions\\*$"
                             "^\\*Quail Completions\\*$"
                             "^\\*elisp macroexpansion\\*$"
                             "^\\*helm M-x\\*$"
                             "^\\*magit"))
  (add-to-list 'display-buffer-alist
               (cons buffer-regexp-bot
                     (cons #'(lambda (buffer &optional data)
                               (rmg-special-display buffer
                                                    rmg-temporary-window-bot))
                           nil))
               t))

(provide 'rmg-panels)
