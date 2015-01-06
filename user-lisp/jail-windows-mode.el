;;; jail-windows-mode.el --- Attempt to bring sanity to Emacs windows

;; Jail windows
;; Copyright (C) 2014 Robert Grimm
;;
;; Author: Robert Grimm <grimm dot rob at gmail dot com>
;; Maintainer: Robert Grimm <grimm dot rob at gmail dot com>
;; Keywords: windows, display-buffer
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Jail-windows is a mode for attempting to maintain a consistent window layout
;; while working within Emacs. This does not directly prevent creation of new
;; windows or deletion of existing windows; instead, it attempts
;;
;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'dash)
(require 'dash-functional)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup jail-windows nil
  "TODO"
  :group 'windows)
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
(defcustom jail-windows-default-layout 'code
  "Default layout to activate when none has been selected and jail-windows-mode
activates for the first time on a frame."
  :group 'jail-windows)
(defcustom jail-windows-new-window-buffer "*scratch*"
  "Default buffer to display when activating a layout with more windows than
were previously shown."
  :type 'string
  :group 'jail-windows)
(defcustom jail-windows-repeat-minimum-horz 45
  "TODO"
  :type 'integer
  :group 'jail-windows)
(defcustom jail-windows-repeat-minimum-vert 3
  "TODO"
  :type 'integer
  :group 'jail-windows)
(defcustom jail-windows-choose-window-actions
  '(jail-windows--single-window-action
    jail-windows--existing-window-action
    jail-windows--same-window-action
    jail-windows--just-pick-one-action)
  "List of functions to select a window to display a buffer.

Functions should use a signature of (fn FRAME WINDOW-GROUP AVAILABLE-WINDOWS
BUFFER ALIST); where the arguments are:

 FRAME -- the frame from which the AVAILABLE-WINDOWS have been chosen,
 WINDOW-GROUP -- the layout group which matched for the BUFFER,
 AVAILABLE-WINDOWS -- a list of windows preselected by jail-windows-mode,
 BUFFER -- the buffer that will be displayed, and
 ALIST -- as described by `display-buffer'.

Each function should return either a window or nil. Note that a returned window
needn't be one from AVAILABLE-WINDOWS.

These functions should only perform the selection of a window and should not
perform the actual buffer-switching behavior."
  :type '(repeat function)
  :group 'jail-windows)
(defcustom jail-windows-debug-messages '(on-off)
  "TODO"
  :type '(set (const :tag "on-off TODO" on-off)
              (const :tag "layout TODO" layout)
              (const :tag "selection TODO" selection)
              (const :tag "selection verbose TODO" selection-verbose))
  :group 'jail-windows)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non-customizable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst jail-windows--real-repeat-minimum-horz 1
  "TODO")
(defconst jail-windows--real-repeat-minimum-vert 1
  "TODO")

;; Hooks
(defvar jail-windows-mode-hook '()
  "Hooks to run when the jail-windows-mode is enabled or disabled.")
(defvar jail-windows-mode-in-hook '()
  "Hooks to run when the jail-windows-mode is enabled.")
(defvar jail-windows-mode-out-hook '()
  "Hooks to run when the jail-windows-mode is disabled.")

;; Layouts
(defvar jail-windows--registered-layouts '())

;; Code layout inspired by
;; http://fullofsta.rs/2012/01/an-improved-emacs-window-setup/
(defconst jail-windows--builtin-layouts-code
  '((group-defs (help "^\\*Apropos\\*$"
                      "^\\*Backtrace\\*$"
                      "^\\*Colors\\*$"
                      "^\\*Compile-Log\\*$"
                      "^\\*Customize "
                      "^\\*ediff.*\\*$"
                      "^\\*e?shell"
                      "^\\*Help\\*$"
                      "^\\*h?grep\\*$"
                      "^\\*info\\*$"
                      "^\\*js\\*$"
                      "^\\*local variables\\*$"
                      "^\\*Locate\\*$"
                      "^\\*magit-\\(diff\\|commit\\)"
                      "^\\*Man "
                      "^\\*Occur\\*$"
                      "^\\*Pp Eval Output\\*$"
                      "^\\*\\(Wo\\)?Man"
                      "^COMMIT_EDITMSG$")
                (completions "Completions\\*$"
                             "^\\*elisp macroexpansion\\*$"
                             "^\\*helm\\(-\\| \\)"
                             "^\\*magit"
                             "^\\*Process List\\*$"
                             "^\\*vc-dir\\*$"))
    (window-layout (| (size . 80)
                      (repeat . t))
                   (- (ratio . 0.66)
                      (groups . (help)))
                   (* (groups . (completions))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Foundation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun jail-windows/active-p (&optional frame)
  "Return t when jail-windows-mode is active; otherwise, nil"
  (and (jail-windows--layout-active-p frame)
       (jail-windows--frame-active-p frame)))

(defun jail-windows/layout-p (layout-name)
  "Return layout name if the layout has been registered; otherwise, nil"
  (if (assq layout-name jail-windows--registered-layouts)
      layout-name
    nil))

(defun jail-windows--get-option (frame option)
  "[Internal] Retrieve a frame-local value."
  (cdr (assq option (frame-parameter frame 'jail-windows-options))))

(defun jail-windows--set-option (frame key value)
  "[Internal] Set a frame-local value."
  (let* ((options (frame-parameter frame 'jail-windows-options))
         (option (assq key options)))
    (if option
        (setcdr option value)
      (push (cons key value) options))
    (modify-frame-parameters frame
                             `((jail-windows-options . ,options)))))

(defun jail-windows--prompt-for-layout (prompt
                                        &optional predicate initial-input)
  "[Internal] Prompt the user for a layout."
  (let* ((choices (list (-map #'car jail-windows--registered-layouts)))
         (choice (completing-read prompt choices predicate t initial-input)))
    (if (equal choice "")
        (caar choices)
      (intern choice))))

(defun jail-windows--message (type &rest args)
  "[Internal] Debug messaging."
  (when (let ((-compare-fn #'eq)) (-contains? jail-windows-debug-messages type))
    (apply #'message args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Layout Registration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jail-windows/register-layout (layout-name layout-def)
  "Register a new layout

TODO: Describe layout

Remember that 'unassigned is a special group name and can't be directly
assigned."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Layout Activation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jail-windows--activate-handle-same (option-alist)
  "[Internal] Handle window layout activation for setting options on a window
without performing any split."
  (let-alist option-alist
    ;; Ensure that groups is a list, and filter out any attempt to directly
    ;; assign 'unassigned as a group. Then set 'unassigned if there are no
    ;; groups to assign
    (setq .groups (or (--remove (eq 'unassigned it)
                                (if (listp .groups)
                                    .groups
                                  (list .groups)))
                      '(unassigned)))

    ;; Set the window parameter so groups persist configuration changes
    (set-window-parameter (selected-window) 'jail-windows-groups .groups)

    ;; Add current window to any groups specified, and to 'unassigned
    ;; otherwise
    (--each .groups
      (let ((group (assq it window-groups)))
        (if group
            (push (selected-window) (cdr group))
          (push (cons it (list (selected-window))) window-groups))))))

(defun jail-windows--activate-handle-verthorz (horizontal option-alist)
  "[Internal] Handle window layout activation for vertical and horizontal
splits."
  (if (or (eq horizontal 'vert)
          (eq horizontal 'vertical)
          (eq horizontal nil))
      (setq horizontal nil)
    (setq horizontal t))
  (let ((first-run t)
        (new-window)
        (requested-size)
        (window-size-fun (if horizontal
                             #'window-body-width
                           #'window-text-height))
        (window-min-size (if horizontal
                             (max jail-windows-repeat-minimum-horz
                                  jail-windows--real-repeat-minimum-horz)
                           (max jail-windows-repeat-minimum-vert
                                jail-windows--real-repeat-minimum-vert)))
        (split-position (if horizontal
                            'right
                          'below)))
    (let-alist option-alist
      (setq requested-size (or .size
                               (when .ratio
                                 (round (* (funcall window-size-fun)
                                           .ratio)))))
      (while (or first-run
                 (and .repeat
                      (or (and .size
                               (<= window-min-size
                                   (- (funcall window-size-fun)
                                      .size)))
                          (and .ratio
                               (<= window-min-size
                                   (* (funcall window-size-fun)
                                      (- 1.0 .ratio)))))))
        (setq first-run nil
              new-window (split-window nil
                                       requested-size
                                       split-position))

        ;; Reuse the "same" handler to set options like window group
        (jail-windows--activate-handle-same option-alist)

        (window-resize nil
                       (- requested-size (funcall window-size-fun))
                       horizontal
                       t)
        (select-window new-window 'norecord)))))

(defun jail-windows--activate-handle-prev (option-alist)
  "[Internal] Handle window layout activation for selecting a previously split
window."
  (unless (< 1 (--count t windows-built))
    (pop windows-built)
    (select-window (car windows-built) 'norecord)

    ;; Reuse the "same" handler to set options like window group
    (jail-windows--activate-handle-same option-alist)))

(defun jail-windows--build-layout-by-name (layout-name)
  "[Internal] Perform the actual layout activation."
  (let ((jail-windows--ignore-conf-change t)
        (ignore-window-parameters t)
        (prev-selected-buffer (window-buffer))
        (prev-active-buffers
         (-map #'window-buffer
               (window-list (selected-frame)
                            'nominibuf
                            (frame-first-window))))
        (layout-def (cdr (assq layout-name
                               jail-windows--registered-layouts)))
        (window-groups)
        (windows-built (list (frame-first-window)))
        (switch-to-window-preserve-window-point t)
        (selected-window)
        (used-windows '()))
    (select-window (car windows-built) 'norecord)
    (switch-to-buffer (or jail-windows-new-window-buffer
                          "*scratch*")
                      'norecord
                      'force-same-window)
    (delete-other-windows (car windows-built))
    (let-alist layout-def
      ;; Build the layout
      (--each .window-layout
        (funcall
         (cl-case (car it)
           (* #'jail-windows--activate-handle-same)
           (| (-partial #'jail-windows--activate-handle-verthorz 'horz))
           (- (-partial #'jail-windows--activate-handle-verthorz 'vert))
           (^ #'jail-windows--activate-handle-prev))
         (cdr it))
        (unless (eq (car windows-built) (selected-window))
          (push (selected-window) windows-built)))

      ;; Set windows against group definitions
      (let* ((-compare-fn nil)
             (group-names (-uniq (-union (-map #'car .group-defs)
                                         (-map #'car window-groups)))))
        (jail-windows--set-option nil
                                  'groups
                                  (--map `(,(cons 'name it)
                                           ,(cons 'matchers
                                                  (cdr (assq it
                                                             .group-defs)))
                                           ,(cons 'windows
                                                  (cdr (assq it
                                                             window-groups))))
                                         group-names)))

      ;; Try to show all buffers that were previously showing
      (--each prev-active-buffers
        (setq selected-window
              (jail-windows--find-window-for-buffer nil
                                                    it
                                                    '()
                                                    used-windows))
        (when selected-window
          (push selected-window used-windows)
          (select-window selected-window 'norecord)
          (switch-to-buffer it 'norecord 'force-same-window)))

      ;; Try to select the previously selected buffer; otherwise select the
      ;; first window in the frame
      (select-window (or (get-buffer-window prev-selected-buffer
                                            (selected-frame))
                         (frame-first-window))
                     'norecord)))
  (jail-windows--update-modeline t))

;;;###autoload
(defun jail-windows/activate-layout (layout-name &optional activate-mode)
  "Activate a jail-windows layout that has been registered with LAYOUT-NAME. If
ACTIVATE-MODE is non-nil, this function will activate jail-windows-mode on the
selected frame."
  (interactive (list (jail-windows--prompt-for-layout "Layout to activate: ")
                     (unless (jail-windows/active-p)
                       (yes-or-no-p
                        "Activate jail-windows-mode in this frame? "))))
  (unless (jail-windows/layout-p layout-name)
    (signal 'wrong-type-argument `(jail-windows/layout-p ,layout-name)))
  (jail-windows--set-option nil 'active-layout layout-name)
  (if (jail-windows--frame-active-p)
      (jail-windows--build-layout-by-name layout-name)
    (when activate-mode (jail-windows-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Layout retainment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jail-windows--layout-active-p (&optional frame)
  "[Internal] Test the windows listed in the activated groups against the live
windows of the frame.

Returns one of the following symbols:

 extra -- All groups refer to live windows, and frame has additional windows
          beyond the windows created by layout
 t -- Windows match exactly
 partial -- Some groups refer to dead windows
 nil -- No groups contain any live windows"
  (let* ((groups (jail-windows--get-option frame 'groups))
         (group-windows (--reduce-from (-union acc (cdr (assq 'windows
                                                              it)))
                                       '()
                                       groups))
         (dead-group-windows (-remove #'window-live-p group-windows))
         (live-group-windows (-filter #'window-live-p group-windows)))
    (if (= 0 (--count t live-group-windows))
        nil
      (if (< 0 (--count t dead-group-windows))
          'partial
        (let* ((frame-windows (window-list frame 'nominibuf))
               (-compare-fn #'eq)
               (frame-only-windows (-difference frame-windows group-windows)))
          (if (< 0 (--count t frame-only-windows))
              'extra
            t))))))

(defun jail-windows--update-modeline (&optional layout-active-p)
  "[Internal] Set the modeline display text based on active state."
  (setq jail-windows-mode
        (when (and (jail-windows--frame-active-p)
                   (if layout-active-p
                       layout-active-p
                     (setq layout-active-p (jail-windows--layout-active-p))))
          (prog1
              (concat jail-windows--modeline-base
                      (cl-case layout-active-p
                        (extra "+")
                        (t "=")
                        (partial "-")))
            (jail-windows--set-option nil
                                      'last-layout-active
                                      layout-active-p)))))

(defun jail-windows--window-config-change-hook-fun ()
  "[Internal] Hook function for `window-configuration-change-hook' to determine
and log whether a window configuration change has "
  (unless (or (not (jail-windows--frame-active-p))
              (and (boundp 'jail-windows--ignore-conf-change)
                   jail-windows--ignore-conf-change))
    (let ((layout-active-p (jail-windows--layout-active-p))
          (last-active-p (jail-windows--get-option nil 'last-layout-active)))
      (unless (eq last-active-p layout-active-p)
        (jail-windows--message 'layout
                               "Jailed window configuration changed! %s --> %s"
                               last-active-p
                               layout-active-p)
        (jail-windows--update-modeline layout-active-p)))))

;; TODO(rgrimm): Advise set-window-configuration to rebuild groups after it has
;; been called

(defun jail-windows--popwin-rebuild-groups (replicate-window-config-output)
  "[Internal] Rebuild group window lists after popwin destroys/recreates them
all.

REPLICATE-WINDOW-CONFIG-OUTPUT takes the output from
`popwin:replicate-window-config' which is a list of cons cells mapping old
windows to new windows."
  (let ((groups (jail-windows--get-option nil 'groups))
        (window-map (--remove (eq (car it) (cdr it))
                              replicate-window-config-output)))
    ;; Copy groups stored on windows (read all parameters before setting any)
    (--each (--map (cons it
                         (window-parameter (car it)
                                           'jail-windows-groups))
                   window-map)
      (set-window-parameter (cdr (car it))
                            'jail-windows-groups
                            (cdr it)))

    ;; Update frame-specific group lists
    (--each groups
      (let ((group-name (cdr (assq 'name it)))
            (windows-cons (assq 'windows it)))
        (setcdr windows-cons
                (--map (let* ((mapped-window (cdr (assq it window-map))))
                         (if mapped-window
                             mapped-window
                           it))
                       (cdr windows-cons)))))
    (jail-windows--set-option nil 'groups groups))
  replicate-window-config-output)

(eval-after-load 'popwin
  `(defadvice popwin:replicate-window-config (around jail-windows-hook activate)
     "Remaps jail-windows group configurations according to the map
returned by `popwin:replicate-window-config'."
     (if (and (boundp 'jail-windows--already-advising-popwin)
              jail-windows--already-advising-popwin)
         (progn ad-do-it)
       (let ((jail-windows--already-advising-popwin t)
             (jail-windows--ignore-conf-change t))
         ad-do-it
         (condition-case e
             (jail-windows--popwin-rebuild-groups ad-return-value)
           (error
            (jail-windows--message 'layout
                                   "[jail-windows] popwin rebuild failed: %s"
                                   e)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jail-windows--group-buffer-compare-fn (buffer buffer-matches-p)
  "[Internal] Function to test buffer against group definition. This will be
used as `-compare-fn' from package `dash'."
  (let ((ret nil))
    (when (eq t buffer-matches-p)
      (setq ret t))
    (when (and (not ret)
               (stringp buffer-matches-p))
      (setq ret (string-match-p buffer-matches-p (buffer-name buffer))))
    (when (and (not ret)
               (functionp buffer-matches-p))
      (setq ret (funcall buffer-matches-p buffer)))
    ret))

(defun jail-windows--single-window-action (frame
                                           window-group
                                           available-windows
                                           buffer
                                           alist)
  "[Internal] Function for inclusion in `jail-windows-choose-window-actions'.
This function will select a window if it is the only element of
AVAILABLE-WINDOWS. If `inhibit-same-window' is set in ALIST, WINDOW-GROUP is
nil, and the only window of AVAILABLE-WINDOWS is the currently selected window,
this function will return nil."
  (when (and (= 1 (--count t available-windows))
             (not (and (cdr (assq 'inhibit-same-window alist))
                       (eq nil window-group)
                       (eq (car available-windows)
                           (frame-selected-window frame)))))
    (car available-windows)))

(defun jail-windows--existing-window-action (frame
                                             window-group
                                             available-windows
                                             buffer
                                             alist)
  "[Internal] Function for inclusion in `jail-windows-choose-window-actions'.
This function will select a window if BUFFER is already displayed within one of
the AVAILABLE-WINDOWS. This will respect the `inhibit-same-window' value from
ALIST as described by `display-buffer'."
  (let ((buffer-window (get-buffer-window buffer (or frame (selected-frame))))
        (-compare-fn nil))
    (when (and buffer-window
               (-contains? available-windows buffer-window)
               (not (and (cdr (assq 'inhibit-same-window alist))
                         (eq buffer-window (frame-selected-window frame)))))
      buffer-window)))

(defun jail-windows--same-window-action (frame
                                         window-group
                                         available-windows
                                         buffer
                                         alist)
  "[Internal] Function for inclusion in `jail-windows-choose-window-actions'.
This function will select `selected-window' if it is available in
AVAILABLE-WINDOWS, unless ALIST contains `inhibit-same-window' as described in
`display-buffer'."
  (let ((-compare-fn nil))
    (when (and (not (cdr (assq 'inhibit-same-window alist)))
               (-contains? available-windows (frame-selected-window frame)))
      (frame-selected-window frame))))

(defun jail-windows--just-pick-one-action (frame
                                           window-group
                                           available-windows
                                           buffer
                                           alist)
  "[Internal] Function for inclusion in `jail-windows-choose-window-actions'.
This function will select the first of AVAILABLE-WINDOWS that satisfies the
options specified within ALIST."
  (let-alist alist
    (let (selected-window)
      (--each-while available-windows
          (not selected-window)
        (when (and
               ;; Check inhibit-same-window
               (not (and .inhibit-same-window
                         (eq it
                             (frame-selected-window frame))))
               ;; Technically window-height is for new windows; so this will
               ;; check whether the existing window is tall enough
               (or (not (integerp .window-height))
                   (<= .window-height
                       (window-text-height it)))
               ;; Again, not technically a requirement of existing windows;
               ;; still, this checks whether the window is wide enough
               (or (not (integerp .window-width))
                   (<= .window-width
                       (window-body-width it)))
               ;; TODO(rgrimm): Floating point versions of window-height and
               ;; window-width.
               )
          (setq selected-window it))
        ;; TODO(rgrimm): Run function versions of window-height and
        ;; window-width against the selected window?
        )
      selected-window)))

(defun jail-windows--find-window-for-buffer (frame
                                             buffer
                                             alist
                                             &optional skip-windows)
  "[Internal] Main logic to select a window for a BUFFER among the windows of
FRAME. If FRAME is nil, the selected frame will be used. This function doesn't
directly use ALIST; instead, it is passed along to the functions of
`jail-windows-choose-window-actions'.

The optional SKIP-WINDOWS should be a list of windows to remove from
consideration."
  (unless frame
    (setq frame (selected-frame)))
  (let ((available-windows (-difference (window-list frame 'nominibuf)
                                        skip-windows))
        (matched-group)
        (selected-window))
    ;; For each group defined in the jail-windows' window-options
    (--each-while (jail-windows--get-option frame 'groups)
        ;; Stop iterating when a group has matched
        (not matched-group)
      ;; Pull the group definition and the window list from the group list
      (let-alist it
        ;; Don't alter available windows if the group is missing either
        ;; matchers or windows
        (when (and (< 0 (-count #'window-live-p .windows))
                   (< 0 (--count t .matchers)))
          ;; Check if this group matches against the buffer
          (if (let ((-compare-fn #'jail-windows--group-buffer-compare-fn))
                (-contains? .matchers buffer))
              ;; When it does match, only use windows from this group
              (progn
                (jail-windows--message 'selection-verbose
                                       "Buffer %s matched group \"%s\""
                                       buffer
                                       .name
                                       .matchers)
                (setq matched-group .name)
                (setq available-windows
                      (let ((-compare-fn nil))
                        (-intersection available-windows .windows))))
            ;; When it doesn't, remove this group's windows from the list
            (setq available-windows
                  (let ((-compare-fn nil))
                    (-difference available-windows .windows)))))))

    ;; Pick among the remaining windows
    (if available-windows
        (progn
          (jail-windows--message 'selection-verbose
                                 "Available windows: %s"
                                 available-windows)

          ;; Defer available windows to list in
          ;; jail-windows-choose-window-actions
          (--each-while (if (listp jail-windows-choose-window-actions)
                            jail-windows-choose-window-actions
                          '())
              (not selected-window)
            (setq selected-window
                  (if (window-live-p it)
                      it
                    (funcall it
                             frame
                             matched-group
                             available-windows
                             buffer
                             alist)))
            (jail-windows--message 'selection-verbose
                                   "%s on %s --> %s"
                                   it
                                   buffer
                                   selected-window))

          (jail-windows--message 'selection
                                 "Window selected for buffer %s --> %s"
                                 buffer
                                 selected-window)

          ;; Return the selected window, if any
          selected-window)
      (jail-windows--message 'selection
                             "No windows available for buffer %s"
                             buffer)
      nil)))

(defun jail-windows--display-buffer-override (buffer alist)
  "[Internal] Function to be used as `display-buffer-overriding-action' when
jail-windows-mode is active. This defers most window selection logic to
`jail-windows--find-window-for-buffer'."
  (let ((selected-window (jail-windows--find-window-for-buffer (selected-frame)
                                                               buffer
                                                               alist)))
    (when selected-window
      (window--display-buffer buffer
                              selected-window
                              'reuse
                              alist))))

(defadvice display-buffer (around jail-window-hook activate)
  "[Internal] Binds `jail-windows--display-buffer-override' to the variable
`display-buffer-overriding-action' when jail-windows-mode is active on the
selected frame."
  (if (jail-windows/active-p)
      (let ((display-buffer-overriding-action
             '((jail-windows--display-buffer-override))))
        ad-do-it)
    (progn ad-do-it)))

;; TODO: (defadvice display-buffer-other-frame ...)?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode toggling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jail-windows--frame-active-p (&optional frame)
  "[Internal] Read whether jail-windows-mode should be active from the frame
parameters."
  (frame-parameter frame 'jail-windows-mode))

(defun jail-windows--set-frame-active (state &optional frame)
  "[Internal] Set frame parameter for whether jail-windows-mode should be
active."
  (modify-frame-parameters nil (list (cons 'jail-windows-mode state))))

(defvar jail-windows--modeline-base " JW"
  "[Internal] Base of modeline text.")
(defvar jail-windows-mode nil
  "Indicator variable for variable `minor-mode-alist'.")

(push '(jail-windows-mode jail-windows-mode)
      minor-mode-alist)

;;;###autoload
(define-minor-mode jail-windows-mode
  "Jail the windows to a set layout."
  :global t
  :variable ((jail-windows/active-p) .
             (lambda (state)
               (jail-windows--set-frame-active state)))
  (when (jail-windows--frame-active-p)
    (add-hook 'window-configuration-change-hook
              #'jail-windows--window-config-change-hook-fun
              t)

    (unless (cdr (assq 'jail-windows-groups
                       window-persistent-parameters))
      (push '(jail-windows-groups . writable)
            window-persistent-parameters))

    (jail-windows/activate-layout
     (or (jail-windows--get-option nil
                                   'active-layout)
         (jail-windows/layout-p jail-windows-default-layout)
         (caar jail-windows--registered-layouts))))
  (run-hooks (when (jail-windows/active-p)
               'jail-windows-mode-in-hook)
             'jail-windows-mode-hook
             (unless (jail-windows/active-p)
               'jail-windows-mode-out-hook))
  (jail-windows--message 'on-off
                         "jail-windows-mode %sabled"
                         (if (jail-windows/active-p)
                             "en"
                           "dis")))

(provide 'jail-windows-mode)
