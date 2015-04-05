(use-package clojure-mode
             :init
             (add-to-list 'auto-mode-alist '("\\.clj(x|s)?$"  . clojure-mode))
             (add-to-list 'auto-mode-alist '("\\.boot$"  . clojure-mode))
             (add-to-list 'auto-mode-alist '("\\.edn$"  . clojure-mode))
             :config
             (progn
               (add-hook 'clojure-mode-hook
                         (lambda ()
                           (push '("fn"  . ?λ) prettify-symbols-alist)
                           (push '("=/=" . ?≢) prettify-symbols-alist)
                           (push '("==" . ?≡) prettify-symbols-alist)
                           (push '("not=" . ?≠) prettify-symbols-alist)
                           (push '("->" . ?→) prettify-symbols-alist)
                           (push '("->>" . ?⇉) prettify-symbols-alist)
                           (push '("<=" . ?≤) prettify-symbols-alist)
                           (push '(">=" . ?≥) prettify-symbols-alist)
                           (push '(">!" . ?⤻) prettify-symbols-alist)
                           (push '("<!" . ?⤺) prettify-symbols-alist)))
               (define-clojure-indent
                 (defroutes 'defun)
                 (GET 2)
                 (POST 2)
                 (PUT 2)
                 (DELETE 2)
                 (HEAD 2)
                 (ANY 2)
                 (context 2))))

(use-package cider-mode
             :init
             (setq cider-repl-use-pretty-printing t
                   nrepl-hide-special-buffers t
                   cider-prefer-local-resources t
                   nrepl-log-messages t
                   cider-show-error-buffer 'only-in-repl
                   cider-auto-select-error-buffer nil
                   cider-prompt-save-file-on-load nil
                   cider-stacktrace-default-filters '(tooling dup)
                   cider-stacktrace-fill-column 80
                   nrepl-buffer-name-show-port t
                   cider-repl-result-prefix ";; => "
                   cider-repl-wrap-history t
                   cider-repl-history-size 1000
                   cider-repl-history-file "/tmp/cider-repl.history")
             :config
             (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
             (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode))

(use-package clojure-snippets)

(use-package clj-refactor
             :config
             (add-hook 'clojure-mode-hook
                       (lambda ()
                         (clj-refactor-mode 1)
                         (cljr-add-keybindings-with-prefix "C-c C-a"))))

(provide 'personal/prelude-clojure)
