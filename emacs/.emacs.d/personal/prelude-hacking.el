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
  :bind
  (("TAB" . tab-indent-or-complete)
   ("<tab>" . tab-indent-or-complete))

  :config
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "snippets" prelude-personal-dir))
  (yas/global-mode 1))

;; Exuberant ctags 
(use-package ctags
  :init 
  (setq path-to-ctags "/usr/local/bin/ctags"
        projectile-tags-command "/usr/local/bin/ctags -Re %s %s"
        tags-revert-without-query t)
  :bind
  (("<f7>" . ctags-create-or-update-tags-table)
   ("M-." . ctags-search)))

;; Projectile
(use-package projectile
  :config
  (progn
    (setq projectile-enable-caching t)
    (projectile-global-mode t)
    (diminish 'projectile-mode " ℗")
    (add-hook 'projectile-mode-hook 'projectile-rails-on)))

(use-package helm-mode
  :diminish (helm-mode . "H")
  :bind
  (("M-y" . helm-show-kill-ring))
  :config
  (key-chord-define-global "yy" 'helm-show-kill-ring))

(use-package js-mode
  :config
  (setq auto-mode-alist (cons '("\\.template$" . js-mode) auto-mode-alist)))

;; Restclient
(use-package restclient
  :config
  (setq auto-mode-alist (cons '("\\.http$" . restclient-mode) auto-mode-alist)))

;; Zen Coding
(use-package web-mode
  :init
  (progn
    (defun personal/disable-smartparens ()
      (smartparens-mode 0))

    (defun personal/sp-web-mode-is-code-context (id action context)
      (when (and (eq action 'insert)
                 (not (or (get-text-property (point) 'part-side)
                          (get-text-property (point) 'block-side))))
        t))
    (defun personal/web-mode-hook ()
      "Hooks for Web mode."
      (setq web-mode-markup-indent-offset 2
            web-mode-css-indent-offset 2
            web-mode-code-indent-offset 2
            web-mode-enable-auto-pairing t)
      (add-to-list 'sp-ignore-modes-list 'web-mode)))
  
  :config
  (setq auto-mode-alist (cons '("\\.php$" . web-mode) auto-mode-alist))
  (add-hook 'web-mode-hook  'personal/web-mode-hook)
  (add-hook 'web-mode-hook 'personal/disable-smartparens))

(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode))

;; TAGS management
(use-package ctags-auto-update-mode
  :diminish ctags-auto-update-mode
  :commands ctags-update
  :config
  (add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
  (add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode))

(provide 'personal/prelude-hacking)
