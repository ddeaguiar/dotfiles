(require 'clojure-snippets)

;; Clojure Programming
(setq auto-mode-alist (cons '("\\.edn$"  . clojure-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.clj(x|s)?$" . clojure-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.boot$" . clojure-mode) auto-mode-alist))

(setq cider-repl-use-pretty-printing t)
(setq nrepl-hide-special-buffers t)
(setq cider-prefer-local-resources t)
(setq nrepl-log-messages t)
;;(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-show-error-buffer 'only-in-repl)
(setq cider-auto-select-error-buffer nil)
(setq cider-prompt-save-file-on-load nil)
(setq cider-repl-result-prefix ";; => ")

;; Emacs complains about these being free variables
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 1000)
(setq cider-repl-history-file "/tmp/cider-repl.history")

(defvar clojure--prettify-symbols-alist nil)

;; Make fn look nice
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
            (push '("." . ?•) prettify-symbols-alist)        
            (push '(">!" . ?⤻) prettify-symbols-alist)
            (push '("<!" . ?⤺) prettify-symbols-alist)
            ))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)

(require 'clj-refactor)
(add-hook 'clojure-mode-hook
          (lambda ()
            (clj-refactor-mode 1)
            (cljr-add-keybindings-with-prefix "C-c C-m")))

;; Place the function call requiring `clojure-mode` after the call to `package-initialize`
;; if you are using Emacs version 24.1.1
(require 'clojure-mode)

(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))
