;; Other global display settings
(line-number-mode 1)
(column-number-mode 1)

(blink-cursor-mode -1)

(show-paren-mode 1)
(setq show-paren-style 'mixed)

(global-whitespace-mode 1)
(setq-default whitespace-style '(face
                                 trailing
                                 tabs
                                 lines-tail
                                 empty
                                 indentation
                                 space-after-tab
                                 space-before-tab))

;; Pretty-print eval
(global-set-key [remap eval-expression] 'pp-eval-expression)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

;; Always show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Font lock is syntax highlighting
(global-font-lock-mode 1)

;; Differentiate file buffers
(when (rmg-try-require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; Display search counts
(global-anzu-mode 1)

;; I love the code comments of diminish; yet I had to replace with delight
(when (rmg-try-require 'delight)
  (delight '((anzu-mode nil "anzu")
             (global-whitespace-mode nil "whitespace")
             (guide-key-mode nil "guide-key")
             (helm-mode nil "helm-mode")
             (magit-auto-revert-mode nil "magit")
             (undo-tree-mode nil "undo-tree"))))

;; Set theme -- moe-dark looks very nice on 256terms and in GUI
(when (and (rmg-try-require 'powerline)
           (rmg-try-require 'moe-theme))
  (powerline-moe-theme))
(when (rmg-try-require 'moe-theme)
  (moe-dark))

(provide 'rmg-display-global)
