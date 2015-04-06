(use-package org
  :init
  ;; Export
  (setq org-html-validation-link nil
        org-export-html-postamble nil
        org-export-backends '(ascii html icalendar latex md))
  ;; File handling
  (setq org-directory "~/Dropbox/org"
        org-default-notes-file (concat org-directory "/journal.org")
        org-log-done 'time)
  ;; TODO
  (setq org-todo-keywords
        '((type "NEW" "WIP" "BLOCKED" "PAUSED" "TESTING" "|" "DONE")))
  :config
  '(require 'ox-md nil t)
  (add-hook 'org-mode-hook
            (lambda ()
              (flyspell-mode)
              (writegood-mode)
              (auto-fill-mode))))

(use-package ob
  :config
  (require 'ob)
  (require 'ob-clojure)
  (require 'cider)
  (setq org-babel-clojure-backend 'cider
        org-fontify-done-headline t
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-src-window-setup 'current-window)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sh         . t)
     (clojure    . t)
     (java       . t)
     (js         . t)
     (ruby       . t)
     (python     . t)))
  (add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))
  (add-to-list 'org-babel-tangle-lang-exts '("js"      . "js")))

(provide 'personal/prelude-org)
