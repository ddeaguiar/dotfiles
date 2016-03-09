(prelude-require-packages
 '(git-gutter-fringe+
   powerline
   indicators
   unbound
   dired+
   rainbow-identifiers
   ack
   hydra
   paradox
   ggtags
   multi-term))

(eval-when-compile
  (require 'use-package))

(require 'diminish)
;;(require 'bind-key)
(require 'prelude-helm-everywhere)

(add-to-list 'load-path "/Users/ddeaguiar/src/org-mode/lisp")
(setq use-package-always-ensure t)

(setq user-email-address "ddeaguiar@pointslope.com")
(setq user-full-name "Daniel De Aguiar")
