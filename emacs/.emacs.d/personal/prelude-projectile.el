(prelude-require-packages '(projectile-direnv))

(use-package projectile
  :config
  (setq projectile-enable-caching t)
  (projectile-global-mode t)
  (diminish 'projectile-mode " ℗"))

(use-package projectile-direnv
  :config
  (add-hook 'projectile-mode-hook 'projectile-direnv-export-variables))

(provide 'personal/prelude-projectile)
