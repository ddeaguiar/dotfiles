(require 'prelude-packages)

(prelude-require-packages
 '(use-package
   git-gutter-fringe+
   powerline
   cyberpunk-theme
   smart-mode-line
   writegood-mode
   ctags
   ctags-update

   ;; clojure
   clojure-mode
   clojure-snippets
   datomic-snippets
   cljsbuild-mode
   clj-refactor

   ;; Racket / Scheme
   geiser
   
   ;; misc
   ack
   markdown-mode
   restclient
   coffee-mode
   emmet-mode
   floobits

   ;; ruby
   projectile-rails
   flymake-ruby
   rbenv
   rspec-mode
   rinari
   bundler
   robe))

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)

(add-to-list 'load-path "/Users/ddeaguiar/src/org-mode/lisp")

(setq user-email-address "ddeaguiar@pointslope.com")
(setq user-full-name "Daniel De Aguiar")
