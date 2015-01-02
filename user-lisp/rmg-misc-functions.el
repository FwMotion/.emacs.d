(defun rmg/prev-window ()
  "Perform the opposite operation of (other-window)"
  (interactive)
  (other-window -1))

(defun rmg/move-beginning-of-line-dwim ()
  "Toggles between moving point to the first non-whitespace character, and the
start of the line."
  (interactive)
  (let ((start-position (point)))
    ;; Move to the first non-whitespace character
    (back-to-indentation)

    ;; If position hasn't moved, go to the start of the line
    (when (= (point) start-position)
      (move-beginning-of-line nil))))

(defun rmg/kill-region-or-previous-word ()
  "If a region is active, kill it. Otherwise, kill the previous word."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun rmg/goto-line-with-feedback ()
  "Show line numbers temporarily while prompting for line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun rmg/comint-delchar-or-eof-or-kill-buffer (arg)
  "Delete a character, or end the process, or kill the buffer; all depending
upon context"
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(defun rmg/setup-windows-1 ()
  "Shim to use new jail-windows-mode"
  (interactive)
  (jail-windows/activate-layout 'code t))

(provide 'rmg-misc-functions)
