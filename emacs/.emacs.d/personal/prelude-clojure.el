(use-package clojure-mode
             :init
             (add-to-list 'auto-mode-alist '("\\.clj(x|s)?$"  . clojure-mode))
             (add-to-list 'auto-mode-alist '("\\.boot$"  . clojure-mode))
             (add-to-list 'auto-mode-alist '("\\.edn$"  . clojure-mode))
             :bind
             (("C-i" . align-cljlet))
             :config
             (progn
               ;; (add-hook 'clojure-mode-hook
               ;;           (lambda ()
               ;;             (push '("fn"  . ?λ) prettify-symbols-alist)
               ;;             (push '("=/=" . ?≢) prettify-symbols-alist)
               ;;             (push '("==" . ?≡) prettify-symbols-alist)
               ;;             (push '("not=" . ?≠) prettify-symbols-alist)
               ;;             (push '("->" . ?→) prettify-symbols-alist)
               ;;             (push '("->>" . ?⇉) prettify-symbols-alist)
               ;;             (push '("<=" . ?≤) prettify-symbols-alist)
               ;;             (push '(">=" . ?≥) prettify-symbols-alist)
               ;;             (push '(">!" . ?⤻) prettify-symbols-alist)
               ;;             (push '("<!" . ?⤺) prettify-symbols-alist)))
               (define-clojure-indent
                 (defroutes 'defun)
                 (GET 2)
                 (POST 2)
                 (PUT 2)
                 (DELETE 2)
                 (HEAD 2)
                 (ANY 2)
                 (context 2)))
             :pin melpa-stable)

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
             (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
             (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
             :pin melpa-stable)

(use-package clj-refactor
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c C-a")))
  :pin melpa-stable
  :ensure t)

(use-package clojure-snippets
  :ensure t
  :pin melpa-stable)

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
