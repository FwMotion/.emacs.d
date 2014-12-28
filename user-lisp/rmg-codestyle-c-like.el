;; New line should indent automatically
(when (fboundp 'google-make-newline-indent)
  (add-hook 'c-mode-common-hook
            'google-make-newline-indent))

(when (fboundp 'google-set-c-style)
  (add-hook 'c-mode-hook 'google-set-c-style)
  (add-hook 'c++-mode-hook 'google-set-c-style))

(provide 'rmg-codestyle-c-like)
