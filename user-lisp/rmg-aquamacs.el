;; Note: This file is mostly unused, since I prefer http://emacsformacosx.com/

;; Use same style of keys from non-Aquamacs
(osx-key-mode -1)

;; Don't use symbols like ⌘
(setq ns-use-mac-modifier-symbols nil)

;; Stop opening all kinds of frames
(one-buffer-one-frame-mode -1)
(setq special-display-regexps '())

;; When new frames are opened, they should still be placed intelligently
(smart-frame-positioning-mode 1)

;; Don't go messing with fonts
(aquamacs-autoface-mode -1)
(set-face-attribute 'mode-line nil :inherit 'unspecified)
(set-face-attribute 'echo-area nil :family 'unspecified)

;; Nope.jpg
(cua-mode -1)

;; Make scratch act normally
(setq aquamacs-scratch-file nil
      initial-major-mode 'emacs-lisp-mode)

;; Remove a few startup items
(remove-hook 'after-init-hook 'aquamacs-turn-on-buffer-offer-save-in-scratch)
(remove-hook 'after-init-hook 'aquamacs-notice-user-settings)

;; And a few shutdown items
(setq kill-emacs-query-functions (remove 'aquamacs-ask-to-save-options
                                         kill-emacs-query-functions))
(global-set-key [remap aquamacs-save-buffers-kill-emacs]
                'save-buffers-kill-emacs)

;; Reset cursor types
(rmg-on-frames t nil nil
               (setq cursor-type t
                     cursor-in-non-selected-windows t))

(provide 'rmg-aquamacs)
