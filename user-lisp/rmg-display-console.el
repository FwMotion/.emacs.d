(unless (display-graphic-p)
  (when (rmg-try-require 'color-theme)
    (color-theme-initialize)
    (add-hook 'window-setup-hook #'color-theme-euphoria t)))

(provide 'rmg-display-console)
