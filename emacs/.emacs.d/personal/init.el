(require 'prelude-packages)

(prelude-require-packages
 '(use-package
   git-gutter-fringe+
   powerline
   cyberpunk-theme
   smart-mode-line
   writegood-mode
   geiser
   ctags
   ctags-update
   ;; clojure
   clojure-mode
   clojure-snippets
   datomic-snippets
   cljsbuild-mode
   clj-refactor
   ;; misc
   ack
   markdown-mode
   restclient
   coffee-mode
   emmet-mode
   floobits
   ;; ruby
   inf-ruby
   flymake-ruby
   projectile-rails
   rbenv))

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)

(add-to-list 'load-path "/Users/ddeaguiar/src/org-mode/lisp")

(setq user-email-address "ddeaguiar@pointslope.com")
(setq user-full-name "Daniel De Aguiar")
