;;; General programming and language settings not
;;; deserving of their own files

(add-hook 'after-init-hook 'global-company-mode)

(use-package yasnippet
  :diminish (yas-minor-mode . " Ŷ")
  :init
  (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  ;; bind to TAB in keybindings file
  (defun tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (or (not yas/minor-mode)
              (null (do-yas-expand)))
          (if (check-expansion)
              (company-complete-common)
            (indent-for-tab-command)))))

  :config
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "snippets" prelude-personal-dir))
  (yas/global-mode 1))

;; Projectile
(use-package projectile
  :config
  (progn
    (setq projectile-enable-caching t)
    (projectile-global-mode t)
    (diminish 'projectile-mode " ℗")
    ))

(use-package helm-mode
  :diminish (helm-mode . "H")
  :bind
  (("M-y" . helm-show-kill-ring))
  :config
  (helm-autoresize-mode t)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action)
  (setq helm-recentf-fuzzy-match    t
        helm-M-x-fuzzy-match t
        helm-grep-default-command "ack -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack -H --no-group --no-color %e %p %f")
  (key-chord-define-global "yy" 'helm-show-kill-ring))

(use-package js-mode
  :config
  (setq auto-mode-alist (cons '("\\.template$" . js-mode) auto-mode-alist)))

;; TAGS management
(use-package ctags
  :init
  (setq path-to-ctags "/usr/local/bin/ctags"
        projectile-tags-command "/usr/local/bin/ctags -Re %s %s"
        tags-revert-without-query t)
  :bind
  (("<f7>" . ctags-create-or-update-tags-table)
   ("M-." . ctags-search)))

(use-package ctags-update)

(use-package ctags-auto-update-mode
  :diminish ctags-auto-update-mode
  :commands ctags-update
  :config
  (add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
  (add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode))

;; Code analysis
(use-package xcscope
  :config (add-hook 'ruby-mode-hook 'cscope-setup))

(use-package helm-cscope-mode
  :config (add-hook 'ruby-mode-hook 'helm-cscope-mode))

(provide 'personal/prelude-hacking)
