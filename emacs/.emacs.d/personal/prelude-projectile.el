(use-package projectile
  :config
  (progn
    (setq projectile-enable-caching t)
    (projectile-global-mode t)
    (diminish 'projectile-mode " ℗")))

(provide 'personal/prelude-projectile)
