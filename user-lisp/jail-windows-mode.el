(require 'dash)
(require 'dash-functional)

;;; Customization
(defcustom jail-windows-register-builtin-layouts t
  "Flag to determine whether jail-windows-mode should automatically register
the builtin layouts."
  :type 'boolean
  :set (lambda (_sym val)
         (set-default _sym val)
         (when (fboundp 'jail-windows/register-builtin-layouts)
           (if val
               (jail-windows/register-builtin-layouts)
             (jail-windows/unregister-builtin-layouts))))
  :group 'jail-windows)
(defcustom jail-windows-default-layout nil
  "Default layout to activate when none has been selected and jail-windows-mode
activates for the first time on a frame"
  :group 'jail-windows)

;; Hooks
(defvar jail-windows-mode-hook '()
  "Hooks to run when the jail-windows-mode is enabled or disabled.")
(defvar jail-windows-mode-in-hook '()
  "Hooks to run when the jail-windows-mode is enabled.")
(defvar jail-windows-mode-out-hook '()
  "Hooks to run when the jail-windows-mode is disabled.")

;; Layouts
(defvar jail-windows--registered-layouts '())

(defconst jail-windows--builtin-layouts-code
  '((group-defs (help "^\\*Apropos\\*$"
                      "^\\*Backtrace\\*$"
                      "^\\*Colors\\*$"
                      "^\\*Compile-Log\\*$"
                      "^\\*Help\\*$"
                      "^\\*Locate\\*$"
                      "^\\*Man "
                      "^\\*Pp Eval Output\\*$"
                      "^\\*W?o?Man "
                      "^\\*ediff.*\\*$"
                      ;; shell and eshell
                      "^\\*e?shell"
                      ;; js comint
                      "^\\*js\\*$"
                      "^\\*info\\*$"
                      "^\\*local variables\\*$"
                      "^\\*magit-diff")
                (completions "\\*Completions\\*$"
                             "^\\*Helm "
                             "^\\*Ido Completions\\*$"
                             "^\\*Quail Completions\\*$"
                             "^\\*elisp macroexpansion\\*$"
                             "^\\*helm M-x\\*$"
                             "^\\*magit"))
    (window-layout (| (size . 80)
                      (repeat . t))
                   (- (ratio . 0.66)
                      (groups . (help)))
                   (* (groups . (completions))))))

;;; Foundation

;;;###autoload
(defun jail-windows/active-p (&optional frame)
  "Return t when jail-windows-mode is active; otherwise, nil"
  (frame-parameter (or frame (selected-frame))
                   'jail-windows-mode))

(defun jail-windows/layout-p (layout-name)
  "Return layout name if the layout has been registered; otherwise, nil"
  (if (assq layout-name jail-windows--registered-layouts)
      layout-name
    nil))

(defun jail-windows--get-option (frame option)
  "[Internal] Retrieve a frame-local value."
  (cdr (assq option
             (frame-parameter (or frame (selected-frame))
                              'jail-windows-options))))

(defun jail-windows--set-option (frame option value)
  "[Internal] Set a frame-local value."
  (let ((options (frame-parameter (or frame (selected-frame))
                                  'jail-windows-options)))
    (setq options (cons (cons option value)
                        (assq-delete-all option options)))
    (modify-frame-parameters (or frame (selected-frame))
                             `((jail-windows-options . ,options)))))

(defun jail-windows--prompt-for-layout (message
                                        &optional predicate initial-input)
  "[Internal] Prompt the user for a layout."
  (let ((choices (list (-map #'car jail-windows--registered-layouts)))
        (choice))
    (setq choice (completing-read message choices predicate t initial-input))
    (if (equal choice "")
        (car (car choices))
      (intern choice))))

;;; Layout Registration

(defun jail-windows/register-layout (layout-name layout-def)
  "Register a new layout"
  (jail-windows/unregister-layout layout-name)
  (push (cons layout-name layout-def)
        jail-windows--registered-layouts))

(defun jail-windows/unregister-layout (layout-name)
  "Unregister a layout"
  (setq jail-windows--registered-layouts
        (assq-delete-all layout-name
                         jail-windows--registered-layouts)))

(defun jail-windows/register-builtin-layouts ()
  "Register all the built-in layouts"
  (jail-windows/register-layout 'code
                                jail-windows--builtin-layouts-code))

(defun jail-windows/unregister-builtin-layouts ()
  "Unregister all the built-in layouts"
  (-each '(code)
    #'jail-windows/unregister-layout))

(when jail-windows-register-builtin-layouts
  (jail-windows/register-builtin-layouts))

;;; Layout Activation

(defun jail-windows--add-to-groups (window groups)
  "[Internal]"
  (when groups
    (--each (if (listp groups)
                groups
              (list groups))
      (let ((group (or (assq it window-groups)
                       (cons it '()))))
        (push window (cdr group))
        (setq window-groups (cons group
                                  (assq-delete-all it window-groups)))))))

(defun jail-windows--activate-handle-vert (option-alist)
  "[Internal]"
  (let ((first-run t)
        (old-window)
        (new-window)
        (requested-size))
    (let-alist option-alist
      (setq requested-size (or .size
                               (when .ratio
                                 (round (* (window-text-height)
                                           .ratio)))))
      (while (or first-run
                 (and .repeat
                      .size
                      (or .size)
                      (>= (window-text-height) (1+ requested-size))))
        (setq first-run nil
              old-window (selected-window))
        (jail-windows--add-to-groups old-window .groups)
        (setq new-window (split-window old-window
                                       requested-size
                                       'below))
        (window-resize old-window
                       (- requested-size (window-text-height old-window))
                       nil
                       t)
        (select-window new-window t)))))

(defun jail-windows--activate-handle-horz (option-alist)
  "[Internal]"
  (let ((first-run t)
        (old-window)
        (new-window)
        (requested-size))
    (let-alist option-alist
      (setq requested-size (or .size
                               (when .ratio
                                 (round (* (window-body-width)
                                           .ratio)))))
      (while (or first-run
                 (and .repeat
                      .size
                      (>= (window-body-width) (1+ requested-size))))
        (setq first-run nil
              old-window (selected-window))
        (jail-windows--add-to-groups old-window .groups)
        (setq new-window (split-window old-window
                                       requested-size
                                       'right))
        (window-resize old-window
                       (- requested-size (window-body-width old-window))
                       t
                       t)
        (select-window new-window t)))))

(defun jail-windows--activate-handle-same (option-alist)
  "[Internal]"
  (let-alist option-alist
    (jail-windows--add-to-groups (selected-window) .groups)))

(defun jail-windows--activate-handle-prev (&optional option-alist)
  "[Internal]"
  (unless (< 1 (--count t windows-built))
    (pop windows-built)
    (select-window (car windows-built) t)))

;;;###autoload
(defun jail-windows/activate-layout (layout-name &optional activate-mode)
  "Activate a layout."
  (interactive (list (jail-windows--prompt-for-layout "Layout to activate: ")
                     (unless (jail-windows/active-p)
                       (yes-or-no-p
                        "Activate jail-windows-mode in this frame? "))))
  (unless (jail-windows/layout-p layout-name)
    (signal 'wrong-type-argument `(jail-windows/layout-p ,layout-name)))
  (jail-windows--set-option nil 'active-layout layout-name)
  (if (jail-windows/active-p)
      (let ((ignore-window-parameters t)
            (prev-active-buffers (list (-map #'window-buffer
                                             (window-list (selected-frame)
                                                          'nominibuf))))
            (layout-def (cdr (assq layout-name
                                   jail-windows--registered-layouts)))
            (window-groups)
            (windows-built (list (frame-first-window))))
        (select-window (car windows-built) t)
        (delete-other-windows (car windows-built))
        (let-alist layout-def
          ;; Build the layout
          (--each .window-layout
            (funcall
             (cl-case (car it)
               (* #'jail-windows--activate-handle-same)
               (| #'jail-windows--activate-handle-horz)
               (- #'jail-windows--activate-handle-vert)
               (^ #'jail-windows--activate-handle-prev))
             (cdr it))
            (unless (eq (car windows-built) (selected-window))
              (push (selected-window) windows-built)))

          ;; Set windows against regexps
          (jail-windows--set-option nil
                                    'groups
                                    (--map (cons (cdr it)
                                                 (cdr (assq (car it)
                                                            window-groups)))
                                           .group-defs))

          ;; TODO: restore buffers

          (select-window (frame-first-window) t)))
    (when activate-mode (jail-windows-mode 1))))

;;; Core functionality

(defun jail-windows--display-buffer-override (buffer alist)
  "Take over window placement... ALIST is ignored"
  (let ((buffer-name (buffer-name buffer))
        (available-windows (window-list (selected-frame) 'nominibuf))
        (matched-group))
    ;; For each group defined in the jail-windows' window-options
    (--each-while (jail-windows--get-option nil 'groups)
        ;; Stop iterating when a group has matched
        (not matched-group)
      ;; Pull the regexp list and the window list from the group
      (let ((regexp-list (car it))
            (window-list (cdr it))
            (-compare-fn (-flip 'string-match-p)))
        ;; Check if this list's regexp matches the BUFFER argument's name
        (if (-contains? regexp-list buffer-name)
            ;; When it does match, only use windows from this group
            (progn
              (message "Using windows: %s" window-list)
              (setq matched-group t)
              (setq available-windows window-list))
          ;; When it doesn't, remove this group's windows from the list
          (let ((-compare-fn nil))
            (setq available-windows
                  (-difference available-windows window-list))))))

    ;; Filter dead windows
    (setq available-windows
          (-remove (-not #'window-live-p) available-windows))

    ;; Pick among the remaining windows
    (when available-windows
      ;; TODO: Look for an active one
      (car available-windows))))

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
      (let ((display-buffer-overriding-action
                    '(jail-windows--display-buffer-override)))
               ad-do-it)
    (progn ad-do-it)))

;; TODO: (defadvice display-buffer-other-frame ...)?

;;;###autoload
(define-minor-mode jail-windows-mode
  "Jail the windows to a set layout."
  :global t
  :variable ((jail-windows/active-p) .
             (lambda (state)
               (modify-frame-parameters nil `((jail-windows-mode . ,state)))))
  (when (jail-windows/active-p)
    (jail-windows/activate-layout
     (or (jail-windows--get-option nil
                                   'active-layout)
         (jail-windows/layout-p jail-windows-default-layout)
         (car (car jail-windows--registered-layouts)))))
  (run-hooks (when (jail-windows/active-p)
               'jail-windows-mode-in-hook)
             'jail-windows-mode-hook
             (unless (jail-windows/active-p)
               'jail-windows-mode-out-hook))
  (message "jail-windows-mode %sabled"
           (if (jail-windows/active-p)
               "en"
             "dis")))

(provide 'jail-windows-mode)
