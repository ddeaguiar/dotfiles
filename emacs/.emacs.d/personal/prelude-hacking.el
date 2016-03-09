(add-hook 'after-init-hook 'global-company-mode)

(use-package multi-term
  :init
  (setq prelude-term-buffer-name "multi")
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1))))

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

(use-package xcscope
  :config (add-hook 'ruby-mode-hook 'cscope-setup))

(use-package helm-cscope
  :config (add-hook 'ruby-mode-hook 'helm-cscope-mode))

(provide 'personal/prelude-hacking)
