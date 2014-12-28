(when (rmg-try-require 'guide-key)
  ;; Show guides for certain sets of keybind prefixes
  (setq guide-key/guide-key-sequence '("C-x n"
                                       "C-x r"
                                       "C-x v"
                                       "C-x 4"
                                       "C-x 5"
                                       "C-x 6"
                                       "C-x 8"
                                       "C-c"))

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
