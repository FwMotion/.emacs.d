(rmg-on-frames nil nil
               (when (and (not (display-graphic-p))
                          (rmg-try-require 'color-theme))
                 (color-theme-initialize)
                 (rmg-on-frames t t (color-theme-euphoria))))

(provide 'rmg-display-console)
