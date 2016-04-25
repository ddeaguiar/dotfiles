(use-package clojure-mode
             :init
             (add-to-list 'auto-mode-alist '("\\.clj(x|s)?$"  . clojure-mode))
             (add-to-list 'auto-mode-alist '("\\.boot$"  . clojure-mode))
             (add-to-list 'auto-mode-alist '("\\.edn$"  . clojure-mode))
             :bind
             (("C-i" . align-cljlet))
             :config
             (define-clojure-indent
               (defroutes 'defun)
               (GET 2)
               (POST 2)
               (PUT 2)
               (DELETE 2)
               (HEAD 2)
               (ANY 2)
               (context 2)))

(use-package cider
             :init
             (setq cider-repl-use-pretty-printing t
                   nrepl-hide-special-buffers t
                   cider-prefer-local-resources t
                   nrepl-log-messages t
                   cider-show-error-buffer 'except-in-repl
                   cider-repl-pop-to-buffer-on-connect nil
                   cider-auto-select-error-buffer nil
                   cider-prompt-save-file-on-load nil
                   cider-stacktrace-default-filters '(tooling dup)
                   cider-stacktrace-fill-column 80
                   nrepl-buffer-name-show-port t
                   cider-repl-result-prefix ";; => "
                   cider-repl-wrap-history t
                   cider-repl-history-size 1000
                   cider-repl-display-help-banner nil
                   cider-repl-history-file "/tmp/cider-repl.history")
             :config
             (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
             (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode))

(use-package clj-refactor
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c C-a"))))

(use-package clojure-snippets)
(use-package javadoc-lookup)
(use-package clojure-cheatsheet)
(use-package datomic-snippets)
(use-package cljsbuild-mode)
(use-package align-cljlet)



(setq cljr-warn-on-eval nil)

(defun reloaded-repl-reset ()
  (interactive)
  (projectile-save-project-buffers)
  (cider-interactive-eval "(clojure.tools.namespace.repl/refresh)"
                          (cider-insert-eval-handler (cider-current-connection))))

(provide 'personal/prelude-clojure)
