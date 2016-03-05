(prelude-require-packages
 '(use-package
   git-gutter-fringe+
   powerline
   cyberpunk-theme
   flatland-theme
   smart-mode-line
   writegood-mode
   indicators
   unbound
   dired+
   rainbow-identifiers
   ack
   hydra
   paradox
   ggtags))

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)
(require 'prelude-helm-everywhere)

(add-to-list 'load-path "/Users/ddeaguiar/src/org-mode/lisp")

(setq user-email-address "ddeaguiar@pointslope.com")
(setq user-full-name "Daniel De Aguiar")
