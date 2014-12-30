;; Use same style of keys from non-Aquamacs
(osx-key-mode -1)
(setq ns-command-modifier 'super
      ns-alternate-modifier 'meta)

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
(setq after-init-hook (remove 'aquamacs-turn-on-buffer-offer-save-in-scratch
                              after-init-hook))
(setq after-init-hook (remove 'aquamacs-notice-user-settings
                              after-init-hook))

;; And a few shutdown items
(setq kill-emacs-query-functions (remove 'aquamacs-ask-to-save-options
                                         kill-emacs-query-functions))
(global-set-key [remap aquamacs-save-buffers-kill-emacs]
                'save-buffers-kill-emacs)

(provide 'rmg-aquamacs)
