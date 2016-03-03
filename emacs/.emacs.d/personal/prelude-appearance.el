;; No stupid menu bar or scroll bar
(menu-bar-mode -1)
;; (scroll-bar-mode -1)

;; Whitespace, Indentation
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default indent-tabs-mode nil) ;; spaces
(setq prelude-whitespace t)

;; Highlights, Parens
(setq global-hl-line-mode nil)
(setq visible-bell nil)
(setq load-prefer-newer t)

(use-package rainbow-mode
  :pin melpa-stable)

(use-package cyberpunk-theme
  :init
  (add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono-18"))
  (disable-theme 'zenburn)
  (menu-bar-mode -1)
  (line-number-mode t)
  (column-number-mode t)
  (whitespace-mode t)
  (global-prettify-symbols-mode t)
  :config
  (setq prelude-theme 'cyberpunk)
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

;;; Mode Line
(use-package powerline
  :init
  ;; If you have font-rendering issues with powerline
  ;; when running emacs in a terminal, you may need to change
  ;; your Non-ASCII font. See http://input.fontbureau.com/workarounds/
  (setq powerline-arrow-shape 'arrow)
  :config
  (powerline-center-theme)

    (custom-set-faces
     '(powerline-active1 ((t (:foreground "#e0e0e0" :background "#202320"))))
    '(powerline-active2 ((t (:foreground "#b9d977" :background "#353a3d"))))))

;;; Line Numbers
(use-package linum
  :init
  (setq linum-format " %d ")
  :config
  (global-linum-mode t))

(use-package ansi-color
  :config
  (ansi-color-for-comint-mode-on))

(diminish 'company-mode " ©")
(diminish 'flycheck-mode " ✓")
(diminish 'whitespace-mode " Ws")
(diminish 'prelude-mode)

(provide 'personal/prelude-appearance)
