(defcustom rmg-default-use-spaces-to-indent t
  "When non-nil, most defaults will be set to use spaces. When nil, most
defaults will be set to use tabs."
  :type 'boolean
  :group 'rmg)
(defcustom rmg-default-indent-width 2
  "Default size for indent width, except where specific to a language"
  :type 'integer
  :group 'rmg)

(setq-default indent-tabs-mode (not rmg-default-use-spaces-to-indent))

(setq default-tab-width rmg-default-indent-width)
(setq-default tab-width rmg-default-indent-width)

;; Use editorconfig
(editorconfig-mode 1)

(provide 'rmg-codestyle-global)
