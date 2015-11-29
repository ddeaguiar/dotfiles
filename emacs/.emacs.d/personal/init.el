(require 'prelude-packages)

(add-to-list 'package-archives `("melpa-stable" . "http://stable.melpa.org/packages/") t)

(prelude-require-packages
 '(use-package
   git-gutter-fringe+
   powerline
   cyberpunk-theme
   flatland-theme
   smart-mode-line
   writegood-mode
   ctags
   ctags-update
   indicators
   unbound
   dired+

   ;; clojure
   javadoc-lookup
   clojure-cheatsheet
   clojure-snippets
   datomic-snippets
   cljsbuild-mode
   clj-refactor
   rainbow-identifiers
   align-cljlet
   
   ;; Racket / Scheme
   geiser

   ;; misc
   ack

   ;; web
   company-restclient
   restclient
   markdown-mode
   coffee-mode
   less-css-mode
   emmet-mode
   web-mode
   htmlize

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
(require 'prelude-helm-everywhere)

(add-to-list 'load-path "/Users/ddeaguiar/src/org-mode/lisp")

(setq user-email-address "ddeaguiar@pointslope.com")
(setq user-full-name "Daniel De Aguiar")
