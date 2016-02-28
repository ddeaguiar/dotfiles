;; Packages

;; Restclient
(use-package restclient
  :config
  (setq auto-mode-alist (cons '("\\.http$" . restclient-mode) auto-mode-alist)))

(use-package company-restclient)

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

(use-package htmlize)
(use-package markdown-mode)
(use-package coffee-mode)
(use-package less-css-mode)
