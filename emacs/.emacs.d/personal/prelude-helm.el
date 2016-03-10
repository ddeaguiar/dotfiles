(use-package helm
  :diminish (helm-mode . " H")
  :bind
  (("M-y" . helm-show-kill-ring))
  :config
  (helm-autoresize-mode t)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action)
  (setq helm-recentf-fuzzy-match    t
        helm-M-x-fuzzy-match t
        helm-grep-default-command "ack -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack -H --no-group --no-color %e %p %f"))

(provide 'personal/prelude-helm)
