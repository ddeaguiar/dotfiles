(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

(require 'rbenv)
(global-rbenv-mode)



