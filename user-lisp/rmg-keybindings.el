;;; Key settings
;; Mac keys
(when running-on-osx
  (setq ns-alternate-modifier 'meta
        ns-command-modifier 'super
        ns-control-modifier 'control
        ns-function-modifier 'none
        ns-option-modifier 'meta
        ns-right-option-modifier 'left
        ns-right-control-modifier 'left
        ns-right-command-modifier 'left
        ns-right-alternate-modifier 'left))

;;; Global keybindings
;; Preferred window layout
(global-set-key (kbd "C-x C-1") 'rmg/setup-windows-1)
(global-set-key (kbd "C-x C-!") 'rmg/setup-windows-1)
;;(global-set-key (kbd "C-x C-2") 'rmg/setup-windows-2)
;;(global-set-key (kbd "C-x C-@") 'rmg/setup-windows-2)

;; Move between windows
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "M-o") 'rmg/prev-window)

;; WoMan pages on C-h C-w (instead of emacs warranty)
(if (fboundp 'helm-man-woman)
    (global-set-key (kbd "C-h C-w") 'helm-man-woman)
  (global-set-key (kbd "C-h C-w") 'woman))

;; Swap isearch and isearch-regexp
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Go to start of line (Do what I mean!)
(global-set-key (kbd "C-a") 'rmg/move-beginning-of-line-dwim)

;; Goto line (remap)
(global-set-key [remap goto-line] 'rmg/goto-line-with-feedback)

;; Editing
(global-set-key (kbd "C-w") 'rmg/kill-region-or-previous-word)

;; Join lines
(global-set-key (kbd "M-j")
                (lambda()
                  (interactive)
                  (join-line -1)))

;; Set input method (clear C-x C-m for alternate M-x)
(global-set-key (kbd "C-M-\\") 'set-input-method)

;; Webjump
(global-set-key (kbd "C-x g") 'webjump)
(global-set-key (kbd "C-x M-g") 'browse-url-at-point)

;; Make ido-mode lines a little more sane
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "C-o") nil)
            (define-key ido-completion-map (kbd "M-o") nil)
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))

;; Make helm also a bit more sane
(define-key helm-map (kbd "C-o") 'nil)

;;; Conditional Keybindings
;; Find file in project
(when (fboundp 'find-file-in-project)
  (global-unset-key (kbd "C-x f"))
  (global-set-key (kbd "C-x f f") 'find-file-in-project)
  (global-set-key (kbd "C-x f o cc")
                  (ffip-create-pattern-file-finder "*.c" "*.cc" "*.h" "*.cpp"))
  (global-set-key (kbd "C-x f o el")
                  (ffip-create-pattern-file-finder "*.el"))
  (global-set-key (kbd "C-x f o ja")
                  (ffip-create-pattern-file-finder "*.java" "*.aidl"))
  (global-set-key (kbd "C-x f o js")
                  (ffip-create-pattern-file-finder "*.js"))
  (global-set-key (kbd "C-x f o lu")
                  (ffip-create-pattern-file-finder "*.lua"))
  (global-set-key (kbd "C-x f o or")
                  (ffip-create-pattern-file-finder "*.org"))
  (global-set-key (kbd "C-x f o py")
                  (ffip-create-pattern-file-finder "*.py"))
  (global-set-key (kbd "C-x f o sh")
                  (ffip-create-pattern-file-finder "*.sh"))
  (global-set-key (kbd "C-x f o tx")
                  (ffip-create-pattern-file-finder "*.txt" "*.md"))
  (global-set-key (kbd "C-x f o xm")
                  (ffip-create-pattern-file-finder "*.xml")))

;; Magit
(when (fboundp 'magit-status)
  (global-set-key (kbd "C-x C-z") 'magit-status))

;; Org-mode keys
(when (fboundp 'org-agenda)
  (global-set-key (kbd "C-x C-a") 'org-agenda))
(when (fboundp 'org-store-link)
  (global-set-key (kbd "C-c l") 'org-store-link))

;; M-x
(when (fboundp 'helm-M-x)
  ;; Use Steve Yegge's advice to use C-x C-m
  (global-set-key (kbd "C-x C-m") #'helm-M-x)
  ;; C-m gets interpreted as RET
  (global-set-key (kbd "C-x RET") #'helm-M-x)
  (global-set-key (kbd "M-x") #'helm-M-x)
  ;; Store original extended command
  (global-set-key (kbd "C-c C-c M-x") #'execute-extended-command))
(when (fboundp 'smex-major-mode-commands)
  (global-set-key (kbd "M-X") #'smex-major-mode-commands))

;; Help commands
(when (fboundp 'helm-apropos)
  (global-set-key (kbd "C-h a") #'helm-apropos))

;; Buffer switching
(when (fboundp 'helm-mini)
  (global-set-key (kbd "C-x b") #'helm-mini))
(when (fboundp 'helm-buffers-list)
  (global-set-key (kbd "C-x C-b") #'helm-buffers-list))

;; Use <select> as a keybinding for end-of-line (end key over SSH)
(unless (display-graphic-p)
  (global-set-key (kbd "<select>") 'move-end-of-line))

;;; Mode-specific keybindings
;; Additional hideshow hotkeys
(when (fboundp 'hs-minor-mode)
  (add-hook 'hs-minor-mode-hook (lambda ()
                                  (when hs-minor-mode
                                    (define-key hs-minor-mode-map
                                      (kbd "C-c h H") 'hs-hide-all)
                                    (define-key hs-minor-mode-map
                                      (kbd "C-c h S") 'hs-show-all)
                                    (define-key hs-minor-mode-map
                                      (kbd "C-c h h") 'hs-hide-block)
                                    (define-key hs-minor-mode-map
                                      (kbd "C-c h s") 'hs-show-block)
                                    (define-key hs-minor-mode-map
                                      (kbd "C-c h t") 'hs-toggle-hiding)))))

;; Shell
(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'rmg/comint-delchar-or-eof-or-kill-buffer)))

;; Eshell
(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "<tab>") #'helm-esh-pcomplete)
            (define-key eshell-mode-map (kbd "C-c C-l") #'helm-eshell-history)))

;;; Mouse button removal
;; Remap to ignore
(global-set-key [remap mouse-buffer-menu] 'ignore)
(global-set-key [remap mouse-appearance-menu] 'ignore)
(global-set-key [remap kmacro-end-call-mouse] 'ignore)
(global-set-key [remap mouse-set-region] 'ignore)
(global-set-key [remap mouse-drag-region] 'ignore)
(global-set-key [remap mouse-save-then-kill] 'ignore)
(global-set-key [remap mouse-set-secondary] 'ignore)
(global-set-key [remap mouse-drag-secondary] 'ignore)
(global-set-key [remap mouse-start-secondary] 'ignore)
(global-set-key [remap mouse-yank-secondary] 'ignore)
(global-set-key [remap mouse-secondary-save-then-kill] 'ignore)
(global-set-key [remap mouse-split-window-vertically] 'ignore)
(global-set-key [remap mouse-drag-vertical-line] 'ignore)
(global-set-key [remap mouse-split-window-horizontally] 'ignore)
(global-set-key [remap mouse-drag-horizontal-line] 'ignore)
(global-set-key [remap mouse-delete-other-windows] 'ignore)
(global-set-key [remap mouse-delete-window] 'ignore)
(global-set-key [remap mouse-drag-mode-line] 'ignore)
(global-set-key [remap mouse-drag-header-line] 'ignore)

;; And for good measure, remove most bindings as well
(mapcar #'global-unset-key
        (mapcar #'kbd
                '("<double-mouse-1>"
                  "<triple-mouse-1>"
                  "<M-mouse-1>"
                  "<down-mouse-1>"
                  "<C-down-mouse-1>"
                  "<S-down-mouse-1>"
                  "<M-down-mouse-1>"
                  "<drag-mouse-1>"
                  "<M-drag-mouse-1>"

                  "<M-mouse-2>"
                  "<C-down-mouse-2>"

                  "<mouse-3>"
                  "<M-mouse-3>"
                  "<S-mouse-3>"
                  "<C-down-mouse-3>"

                  "<mouse-movement>"

                  "<vertical-line> <mouse-1>"
                  "<vertical-line> <down-mouse-1>"
                  "<vertical-line> <C-mouse-2>"

                  "<vertical-scroll-bar> <C-mouse-2>"

                  "<header-line> <mouse-1>"
                  "<header-line> <down-mouse-1>"

                  "<mode-line> <down-mouse-1>"
                  "<mode-line> <drag-mouse-1>"
                  "<mode-line> <mouse-2>"
                  "<mode-line> <C-mouse-2>"
                  "<mode-line> <mouse-3>")))

(provide 'rmg-keybindings)
