(use-package geiser
  :bind ("C-c M-g" . run-geiser)
  :config
  (setq geiser-active-implementations '(racket)
        geiser-mode-smart-tab-p t
        geiser-repl-startup-time 10000
        geiser-repl-history-filename "~/.emacs.d/geiser-history"
        geiser-repl-query-on-kill-p nil
        geiser-implementations-alist
        '(((regexp "\\.scm$") racket)
          ((regexp "\\.ss$") racket)
          ((regexp "\\.rkt$") racket))))

(provide 'personal/prelude-scheme)
