(defconst rmg-java-style
  `("java"
    (c-offsets-alist . ((arglist-intro . ++)
                        (arglist-cont . 0)
                        (arglist-cont-nonempty . ++)
                        (arglist-close . 0)
                        (case-label . +)
                        (inexpr-class . 0)))))

(add-hook 'java-mode-hook
          (lambda ()
            ;; Add custom style
            (c-add-style "rmg-java" rmg-java-style t)

            ;; Treat Java 1.5 @-style annotations as comments
            (setq c-comment-start-regexp "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

(provide 'rmg-codestyle-java)
