(setq org-directory "~/org")
(setq org-agenda-files (list (concat org-directory "/agenda.org")
                        (concat org-directory "/journal.org")))
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
(setq org-refile-allow-creating-parent-nodes (quote confirm))
(setq org-default-notes-file (concat org-directory "/agenda.org"))
(setq org-special-ctrl-a/e t)
(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)"
                           "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
(setq org-tag-alist '(("@home" . ?h) ("@pointslope" . ?p) ("@mtc" . ?m)
                  ("@ncl" . ?n) ("@dnc" . ?d)))
(setq org-log-done 'time)

;;;; Refile settings (from http://doc.norang.ca/org-mode.html#Refiling)
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)
