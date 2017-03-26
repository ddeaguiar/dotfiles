(prelude-require-package 'use-package)

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)

(require 'prelude-helm-everywhere)

(setq use-package-always-ensure t)

(setq user-email-address "ddeaguiar@gmail.com")
(setq user-full-name "Daniel De Aguiar")

(use-package unbound)
(use-package dired+)
(use-package rainbow-identifiers)
(use-package ack)
(use-package hydra)
(use-package paradox)
(use-package ggtags)

;; I'd rather manage key-chords.
(use-package key-chord
  :config
  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "kk" 'avy-goto-line)
  (key-chord-define-global "jk" 'avy-goto-char-2)
  (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
  (key-chord-define-global "yy" 'browse-kill-ring)
  (key-chord-mode +1))

;;; Mode Line
(use-package powerline
  :init
  (setq powerline-arrow-shape 'arrow)
  :config
  (powerline-center-theme))

;; Require themes
(use-package moe-theme
  :config
  (require 'powerline)
  (moe-dark)
  (moe-theme-set-color 'red)
  (powerline-moe-theme))

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

;; General niceties
(global-prettify-symbols-mode t)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
(add-to-list 'default-frame-alist '(font . "Sauce Code Powerline-16"))

(add-hook 'isearch-mode-end-hook 'recenter-top-bottom)

(defadvice
    isearch-forward
    (after isearch-forward-recenter activate)
  (recenter))
(ad-activate 'isearch-forward)

(defadvice
    isearch-repeat-forward
    (after isearch-repeat-forward-recenter activate)
  (recenter))
(ad-activate 'isearch-repeat-forward)

(defadvice
    isearch-repeat-backward
    (after isearch-repeat-backward-recenter activate)
  (recenter))
(ad-activate 'isearch-repeat-backward)

;;; Line Numbers
(use-package linum
  :init
  (setq linum-format " %d ")
  :config
  (global-linum-mode t))

(use-package ansi-color
  :config
  (ansi-color-for-comint-mode-on))

(use-package company
  :diminish (company-mode . " ©"))

(use-package flycheck
  :diminish (flycheck-mode . " ✓"))

(use-package whitespace
  :diminish (whitespace-mode . " Ws"))

(diminish 'prelude-mode)

;; -- General --

;; UTF-8 Encoding
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq initial-major-mode 'lisp-interaction-mode
      redisplay-dont-pause t
      column-number-mode t
      echo-keystrokes 0.02
      inhibit-startup-message t
      transient-mark-mode t
      shift-select-mode nil
      require-final-newline t
      truncate-partial-width-windows nil
      delete-by-moving-to-trash nil
      confirm-nonexistent-file-or-buffer nil
      query-replace-highlight t
      next-error-highlight t
      next-error-highlight-no-select t
      initial-scratch-message nil)

;; enable cua-mode for rectangular selections
(require 'cua-base)
(require 'cua-gmrk)
(require 'cua-rect)
(cua-mode 1)
(setq cua-enable-cua-keys nil)

;; OS X specific configuration
(setq default-input-method "MacOSX")

;; Make cut and paste work with the OS X clipboard
;; stolen from Emacs Live
(defun live-copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun live-paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(when (not window-system)
  (setq interprogram-cut-function 'live-paste-to-osx)
  (setq interprogram-paste-function 'live-copy-from-osx))

;; Work around a bug on OS X where system-name is a FQDN
(setq system-name (car (split-string system-name "\\.")))

(use-package aggressive-indent)
(use-package ag)

;; Spelling
(use-package flyspell
  :diminish (flyspell-mode " FSp")
  :config
  (progn
    (setq flyspell-issue-welcome-flag nil)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
    (setq-default ispell-list-command "list")))

(use-package ace-window
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package ace-isearch
  :config
  (global-ace-isearch-mode 1))

;; Enable arrow keys
(defun disable-guru-mode ()
  (guru-mode -1))

(add-hook 'prelude-prog-mode-hook 'disable-guru-mode t)

(use-package markdown-mode
;; markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; File handling
(setq backup-directory-alist
      '(("." . "~/.emacs.backups")))

;; Automatically tail log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

(use-package helm
  :diminish (helm-mode . " H")
  :bind
  (("M-y" . helm-show-kill-ring))
  :config
  (helm-autoresize-mode t)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action)
  (setq helm-recentf-fuzzy-match    t
        helm-M-x-fuzzy-match t))

(use-package projectile
  :diminish (projectile-mode . " ℗")
  :config
  (setq projectile-enable-caching t)
  (projectile-global-mode t))

(use-package projectile-direnv
  :config
  (add-hook 'projectile-after-switch-project-hook 'projectile-direnv-export-variables))

;; With Prelude I typically want to edit personal/init.el.
(setq user-init-file (concat prelude-user-init-file "init.el"))

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

;; -- Clojure --

(use-package clojure-mode
             :init
             (add-to-list 'auto-mode-alist '("\\.clj(x|s)?$"  . clojure-mode))
             (add-to-list 'auto-mode-alist '("\\.boot$"  . clojure-mode))
             (add-to-list 'auto-mode-alist '("\\.edn$"  . clojure-mode))
             (setq clojure-align-forms-automatically t)
             :config
             (define-clojure-indent
               (defroutes '(:defn))
               (handler '(:form))
               (component '(:form))
               (context '(:form))
               (with-link '(:form))
               (with-embedded-coll '(:form))
               (http-service-client '(:form))
               (comment '(:form)))
             (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
             (add-hook 'clojure-mode-hook
                       (lambda ()
                         (unbind-key "C-c C-q" aggressive-indent-mode-map))))

(use-package cider
             :init
             (setq cider-repl-use-pretty-printing t
                   nrepl-hide-special-buffers t
                   cider-prefer-local-resources t
                   nrepl-log-messages t
                   cider-show-error-buffer 'except-in-repl
                   cider-repl-pop-to-buffer-on-connect nil
                   cider-auto-select-error-buffer nil
                   cider-prompt-save-file-on-load nil
                   cider-stacktrace-default-filters '(tooling dup)
                   cider-stacktrace-fill-column 80
                   nrepl-buffer-name-show-port t
                   cider-repl-result-prefix ";; => "
                   cider-repl-wrap-history t
                   cider-repl-history-size 1000
                   cider-repl-display-help-banner nil
                   cider-repl-history-file "/tmp/cider-repl.history")
             :config
             (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
             (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
             (advice-add 'cider-find-var :after #'recenter-top-bottom))

(use-package clj-refactor
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c C-a"))))

(use-package clojure-snippets)
(use-package javadoc-lookup)
(use-package clojure-cheatsheet)
(use-package datomic-snippets)
(use-package cljsbuild-mode)

(setq cljr-warn-on-eval nil)

;; -- Scheme --

(use-package geiser
  :bind ("C-c M-g" . run-geiser)
  :config
  (setq geiser-active-implementations '(racket)
        geiser-mode-smart-tab-p t
        geiser-repl-startup-time 10000
        geiser-repl-history-filename "~/.emacs.d/geiser-history"
        geiser-repl-query-on-kill-p nil
        geiser-implementations-alist
        '(((regexp "\\.scm$") racket)
          ((regexp "\\.ss$") racket)
          ((regexp "\\.rkt$") racket))))

;; -- Web --

(use-package htmlize)
(use-package less-css-mode)

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

(use-package js2-mode
  :config
  (setq auto-mode-alist (cons '("\\.template$" . js-mode) auto-mode-alist)))

;; -- Key Bindings --

;;Fix keybindings under iTerm2
(global-set-key "\e[1;9A" [M-up])
(global-set-key "\e[1;9B" [M-down])
(global-set-key "\e[1;9C" [M-right])
(global-set-key "\e[1;9D" [M-left])

(global-set-key (kbd "C-c M-/") 'comment-region)
(global-set-key (kbd "s-<backspace>") 'backward-kill-word)

;; Ctrl-x r i Useful rectangle binding
(global-set-key (kbd "C-x r i") 'string-insert-rectangle)

;; Disable annoying key chords
;;(key-chord-define-global "uu" nil)
;;(key-chord-define-global "lj" nil)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :bind
  (("C-c m l" . mc/edit-lines)
   ("C-c m a" . mc/edit-beginnings-of-lines)
   ("C-c m e" . mc/edit-ends-of-lines)
   ("C-c m h" . mc/mark-all-like-this-dwim)
   ("C-c m t" . mc/mark-sgml-tag-pair)))

;; Window Management
(global-set-key (kbd "M-p") 'fill-paragraph)

;; Frame Management
(global-set-key (kbd "s-o") 'other-frame)
(define-key prelude-mode-map (kbd "s-o") 'other-frame)

(use-package smartparens
  :diminish (smartparens-mode . " (Sm)")
  :config
  (defun personal/delete-horizontal-space ()
    (interactive)
    (just-one-space -1)
    (sp-backward-delete-char))
  (defun personal/just-one-space ()
    (interactive)
    (just-one-space -1))
  (bind-keys
   ("M-\\"             . personal/delete-horizontal-space)
   ("M-SPC"            . personal/just-one-space)
   :map smartparens-mode-map
   ("C-k"              . sp-kill-hybrid-sexp)
   ("C-M-f"            . sp-forward-sexp)
   ("C-M-b"            . sp-backward-sexp)
   ("C-M-d"            . sp-down-sexp)
   ("C-M-a"            . sp-backward-down-sexp)
   ("C-S-a"            . sp-beginning-of-sexp)
   ("C-S-d"            . sp-end-of-sexp)
   ("C-M-e"            . sp-up-sexp)
   ("C-M-u"            . sp-backward-up-sexp)
   ("C-M-t"            . sp-transpose-sexp)
   ("C-M-n"            . sp-next-sexp)
   ("C-M-p"            . sp-previous-sexp)
   ("C-M-k"            . sp-kill-sexp)
   ("C-M-w"            . sp-copy-sexp)
   ("M-<delete>"       . sp-unwrap-sexp)
   ("M-<backspace>"    . sp-backward-unwrap-sexp)
   ("C-<right>"        . sp-forward-slurp-sexp)
   ("C-<left>"         . sp-forward-barf-sexp)
   ("C-M-<left>"       . sp-backward-slurp-sexp)
   ("C-M-<right>"      . sp-backward-barf-sexp)
   ("M-D"              . sp-splice-sexp)
   ("C-M-<delete>"     . sp-splice-sexp-killing-forward)
   ("C-M-<backspace>"  . sp-splice-sexp-killing-backward)
   ("C-S-<backspace>"  . sp-splice-sexp-killing-around)
   ("C-]"              . sp-select-next-thing-exchange)
   ("C-<left_bracket>" . sp-select-previous-thing)
   ("C-M-]"            . sp-select-next-thing)
   ("M-F"              . sp-forward-symbol)
   ("M-B"              . sp-backward-symbol)
   :map emacs-lisp-mode-map
   (")"                . sp-up-sexp))
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'web-mode "<" nil :when '(personal/sp-web-mode-is-code-context))
  (sp-with-modes '(clojure-mode cider-repl-mode)
    (sp-local-pair "`" nil :actions nil))
  (sp-with-modes '(html-mode sgml-mode)
    (sp-local-pair "<" ">")))

;; hydra
;; from https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(defun splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

;; from https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
(defhydra hydra-splitter (global-map "C-M-s")
  "splitter"
  ("h" splitter-left)
  ("j" splitter-down)
  ("k" splitter-up)
  ("l" splitter-right)
  ("q" nil))

;(global-set-key  (kbd "C-M-s") 'hydra-splitter/body)

(defhydra hydra-apropos (:color blue :hint nil)
  ("a" apropos "apropos")
  ("d" apropos-documentation "documentation")
  ("v" apropos-variable "variable")
  ("c" apropos-command "command")
  ("l" apropos-library "library")
  ("u" apropos-user-option "user-option")
  ("e" apropos-value "value"))

(global-set-key (kbd "C-c H") 'hydra-apropos/body)

(defun find-lein-profile ()
  (interactive)
  (find-file-other-window "~/.lein/profiles.clj"))

(defhydra hydra-find-files (:color blue :hint nil)
  ("i" crux-find-user-init-file "init")
  ("l" find-lein-profile "lein profile"))

(global-set-key (kbd "C-c C-,") 'hydra-find-files/body)

;; -- Org --

(use-package org
  :init
  ;; Export
  (setq org-html-validation-link nil
        org-export-html-postamble nil
        org-return-follows-link t
        org-startup-indented t
        org-export-backends '(ascii html icalendar latex md))
  ;; File handling
  (setq org-directory "~/Dropbox/org"
        org-default-notes-file (concat org-directory "/journal.org")
        org-log-done 'time)
  ;; TODO
  (setq org-todo-keywords
        '((type "NEW" "WIP" "BLOCKED" "PAUSED" "TESTING" "|" "DONE")))
  :config
  '(require 'ox-md nil t)
  (add-hook 'org-mode-hook
            (lambda ()
              (flyspell-mode)
              (writegood-mode)
              (auto-fill-mode)
              (smartparens-mode -1)
              (prelude-off))))


; magit-gc-override-mode
(defvar magit-gc-override-author ""
  "Holds the git commit author override.")

(defun my/git-set-author (author)
  "Sets the '--author' argument to the input author."
  (when (not (string= "" author))
    (add-to-list 'magit-commit-arguments (concat "--author=" author))
    (minibuffer-message (concat "Author overridden with '" author "'"))))

(defun my/git-override-author ()
  "Activates a git commit author override using the input author."
  (interactive)
  (let ((author (read-string "i.e., A U Thor <author@example.com>: " magit-gc-override-author)))
    (setq magit-gc-override-author author)
    (my/git-set-author author)))

(defun my/git-remove-author-override ()
  "Removes the '--author' commit argument."
  (interactive)
  (setq magit-commit-arguments
        (remove-if (lambda (s) (string-match "--author" s))
                   magit-commit-arguments))
  (minibuffer-message "Author override removed."))

(defun my/git-author-toggle ()
  "Toggles the git commit author override."
  (if (find-if (lambda (s) (string-match "--author" s)) magit-commit-arguments)
    (my/git-remove-author-override)
    (my/git-set-author magit-gc-override-author)))

(define-minor-mode magit-gc-override-mode
  "Toggle Magit git-commit override mode.
   When enabled, allows git-commit overrides to be specified."
  :init-value nil
  :lighter " GCO"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-p") 'my/git-pair)
            (define-key map (kbd "C-c C-o") 'my/git-unpair)
            map)
  :group 'magit-gc-override
  (my/git-author-toggle))

(diminish 'magit-gc-override-mode " æ")
