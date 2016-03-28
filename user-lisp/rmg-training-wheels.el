(when (rmg-try-require 'guide-key)
  ;; Show guides for certain sets of keybind prefixes
  (setq guide-key/guide-key-sequence '("C-x c" ; helm
                                       "C-x n" ; narrow
                                       "C-x r" ; rectangle
                                       "C-x v" ; VCS
                                       "C-x 4" ; window
                                       "C-x 5" ; frame
                                       "C-x 6" ; 2C 2 column
                                       "C-x 8" ; extended chars
                                       "C-c")) ; mode-specific

  ;; Show recursively
  (setq guide-key/recursive-key-sequence-flag t)

  ;; Show guide at the bottom
  (setq guide-key/popup-window-position 'bottom)

  ;; Don't show guide-key immediately
  (setq guide-key/polling-time 1)

  ;; Enable guide-key mode
  (guide-key-mode 1))

(when (rmg-try-require 'jquery-doc)
  (jquery-doc-setup))

(provide 'rmg-training-wheels)
