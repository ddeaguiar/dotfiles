;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
;;
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(add-to-list 'default-frame-alist              '(fullscreen . maximized))
(setq user-full-name "Daniel De Aguiar"
      user-mail-address "ddeaguiar@gmail.com"
      doom-theme 'doom-tomorrow-night
      doom-font (font-spec :family "Fira Code"
                           :weight 'light
                           :size 14)
      display-line-numbers-type t
      ;; See https://github.com/DarthFennec/highlight-indent-guides
      highlight-indent-guides-method 'column
      backup-directory-alist '(("." . "~/.emacs.backups")))

(defun me/delete-horizontal-space ()
    (interactive)
    (just-one-space -1)
    (sp-backward-delete-char))

(defun me/just-one-space ()
    (interactive)
    (just-one-space -1))

(map! "C-x 5 t" #'toggle-frame-fullscreen
      "C-x g"   #'magit-status
      "C-c M-/" #'comment-region
      "M-%"     #'anzu-query-replace
      "C-M-%"   #'anzu-query-replace-regexp
      "M-\\"    #'me/delete-horizontal-space
      "M-SPC"   #'me/just-one-space
      ;;Fix keybindings under iTerm2
      "\e[1;9A" [M-up]
      "\e[1;9B" [M-down]
      "\e[1;9C" [M-right]
      "\e[1;9D" [M-left])

(map! :map general-override-mode-map
      [remap imenu] #'lsp-ui-imenu)

(map! :map ivy-minibuffer-map
      "C-l" #'ivy-backward-kill-word)

(add-hook! 'prog-mode-hook 'rainbow-identifiers-mode)
(add-hook! 'prog-mode-hook 'smartparens-strict-mode)

;; Automatically tail log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))
(add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))

;; package configs

(use-package! projectile
  :config
  (setq projectile-enable-caching t)
  (add-to-list 'projectile-globally-ignored-directories "project/target"))

(use-package! lsp-mode
  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
     (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (setq lsp-enable-indentation nil
        lsp-clojure-server-command '("bash" "-c" "clojure-lsp")))

(use-package! lsp-ui
 :hook (lsp-mode . lsp-ui-mode)
 :config
 (setq lsp-ui-doc-max-height 20
        lsp-ui-doc-max-width 75))


;; -- Clojure --

;; Start a clojure repl with socket repl support
;; lein: `lein with-profiles +socket,+rebl-jar run'
;; clj: `clj -Asocket:rebl-jar`
(setq inferior-lisp-program "nc localhost 60606")

;; use with caution
;; large buffers are problematic
;; Prefer load file instead
(defun my/lisp-eval-buffer ()
  (interactive)
  (lisp-eval-string (buffer-string)))

;; Originally from Luke
(defun my/clojure-load-file ()
  "Send a load-file instruction to Clojure to load the current file"
  (interactive)
  (comint-proc-query (inferior-lisp-proc)
                     (format "(clojure.core/load-file \"%s\")\n" (buffer-file-name))))
(defun my/clojure-in-ns ()
  "Send a command to the inferior Lisp to enter ns of current file."
  (interactive)
  (comint-proc-query (inferior-lisp-proc)
                     (format "(clojure.core/in-ns '%s)\n" (clojure-find-ns))))

(defun my/clojure-spec-describe (sym)
  "Send a command to the inferior Lisp to describe a spec. Defaults to lisp-var-at-pt"
  (interactive (lisp-symprompt "spec" (lisp-var-at-pt)))
  (comint-proc-query (inferior-lisp-proc)
                     (format "(clojure.pprint/pprint (clojure.spec.alpha/describe %s))\n" sym)))

(defun my/clojure-run-tests ()
  "Send a command to the inferior Lisp to run clojure tests."
  (interactive)
  (comint-proc-query (inferior-lisp-proc)
                     "(clojure.test/run-tests)\n"))

(defun my/rebl-inspect ()
  "Wrap the previous sexp with cognitect.rebl/inspect and send it to the inferior Lisp process."
  (interactive)
  (let ((expr (buffer-substring (save-excursion (backward-sexp) (point)) (point))))
    (let ((str (format "(try
                           (cognitect.rebl/inspect %s)
                           (catch Exception _))\n" expr)))
      (comint-send-string (inferior-lisp-proc) str))))

(setq lisp-describe-sym-command "(clojure.repl/doc %s)\n")

(use-package! clojure-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.clj(x|s)?$"  . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot$"  . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.edn$"  . clojure-mode))
  (setq clojure-align-forms-automatically t)
  :config
  (define-clojure-indent
    (handler '(:form))
    (component '(:form))
    (context '(:form))
    (http-service-client '(:form))
    (comment '(:form))
    (def-specs '(:form))
    (for-all '(:defn)))
  (add-hook 'clojure-mode-hook 'eldoc-mode)
  (add-hook 'clojure-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'clojure-mode-hook 'hi-lock-mode)
  (add-hook 'clojure-mode-hook 'lsp-mode))

(map! :map (clojure-mode-map inferior-lisp-mode-map)
      :localleader
      (:prefix ("c" . "clojure")
       "a" #'my/clojure-spec-describe
       "b" #'my/lisp-eval-buffer
       "c" #'inferior-lisp
       "d" #'lsp-ui-doc-glance
       "D" #'ivy-clojuredocs-at-point
       "e" #'lisp-eval-last-sexp
       "E" #'lisp-eval-form-and-next
       "i" #'my/rebl-inspect
       "j" #'javadoc-lookup
       "l" #'my/clojure-load-file
       "n" #'my/clojure-in-ns
       "r" #'lisp-eval-region
       "s" #'lsp-ivy-workspace-symbol
       "t" #'my/clojure-run-tests
       "z" #'switch-to-lisp))

(javadoc-add-artifacts [org.slf4j slf4j-api 1.7.30]
                       [ch.qos.logback logback-classic 1.2.3]
                       [net.logstash.logback logstash-logback-encoder 6.3]
                       [org.eclipse.jetty jetty-server 9.4.20.v20190813]
                       [org.eclipse.jetty jetty-servlet 9.4.20.v20190813]
                       [com.amazonaws aws-java-sdk 1.7.4.2]
                       [com.amazonaws aws-java-sdk-cloudwatch 1.11.683]
                       [com.amazonaws aws-java-sdk-s3 1.11.683]
                       [com.amazonaws aws-java-sdk-batch 1.11.683]
                       [com.amazonaws aws-xray-recorder-sdk-core 2.5.0]
                       [com.amazonaws aws-xray-recorder-sdk-aws-sdk 2.5.0]
                       [com.amazonaws aws-xray-recorder-sdk-sql 2.5.0]
                       [com.amazonaws aws-xray-recorder-sdk-apache-http 2.5.0])

(use-package! smartparens
  :config
  ;; undo doom config
  ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/config/default/config.el#L100-L104
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'clojure-mode "`" nil :actions nil)
  (sp-local-pair sp-lisp-modes "(" ")" :unless nil)
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil :post-handlers nil :unless nil))
  (map! :map smartparens-mode-map
   "C-M-f"            #'sp-forward-sexp
   "C-M-b"            #'sp-backward-sexp
   "C-M-d"            #'sp-down-sexp
   "C-M-a"            #'sp-backward-down-sexp
   "C-S-a"            #'sp-beginning-of-sexp
   "C-S-d"            #'sp-end-of-sexp
   "C-M-e"            #'sp-up-sexp
   "C-M-u"            #'sp-backward-up-sexp
   "C-M-t"            #'sp-transpose-sexp
   "C-M-n"            #'sp-next-sexp
   "C-M-p"            #'sp-previous-sexp
   "C-M-k"            #'sp-kill-sexp
   "C-M-w"            #'sp-copy-sexp
   "M-<delete>"       #'sp-unwrap-sexp
   "M-<backspace>"    #'sp-backward-unwrap-sexp
   "C-<right>"        #'sp-forward-slurp-sexp
   "C-<left>"         #'sp-forward-barf-sexp
   "C-M-<left>"       #'sp-backward-slurp-sexp
   "C-M-<right>"      #'sp-backward-barf-sexp
   "M-D"              #'sp-splice-sexp
   "C-M-<delete>"     #'sp-splice-sexp-killing-forward
   "C-M-<backspace>"  #'sp-splice-sexp-killing-backward
   "C-S-<backspace>"  #'sp-splice-sexp-killing-around
   "C-]"              #'sp-select-next-thing-exchange
   "C-<left_bracket>" #'sp-select-previous-thing
   "C-M-]"            #'sp-select-next-thing
   "M-F"              #'sp-forward-symbol
   "M-B"              #'sp-backward-symbol))

(use-package! org
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
        '((type "NEW" "WIP" "BLOCKED" "PAUSED" "|" "DONE")))
  :config
  '(require 'ox-md nil t)
  (add-hook 'org-mode-hook
            (lambda ()
              (flyspell-mode)
              (auto-fill-mode)
              (smartparens-mode -1))))

(defhydra hydra-apropos (:color blue :hint nil)
  ("a" apropos "apropos")
  ("d" apropos-documentation "documentation")
  ("v" apropos-variable "variable")
  ("c" apropos-command "command")
  ("l" apropos-library "library")
  ("u" apropos-user-option "user-option")
  ("e" apropos-value "value"))

(defun find-lein-profile ()
  (interactive)
  (find-file-other-window "~/.lein/profiles.clj"))

(defun find-global-git-ignore ()
  (interactive)
  (find-file-other-window "~/.gitignore_global"))

(defun find-deps-edn ()
  (interactive)
  (find-file-other-window "~/.clojure/deps.edn"))

(defhydra hydra-find-files (:color blue :hint nil)
  ("d" find-deps-edn "deps.edn")
  ("g" find-global-git-ignore ".gitignore_global")
  ("l" find-lein-profile "lein profile"))

(global-set-key (kbd "C-c l h") 'hydra-apropos/body)
(global-set-key (kbd "C-c l ,") 'hydra-find-files/body)

;; magit gc overrides
;; Used as a replacement for git pair scripts.
;; It's nice to see the author override with Magit.

(defvar my/magit-gc-override-author ""
  "Holds the git commit author override so it can be toggled..")

(defun my/git-set-author (author)
  "Sets the '--author' argument to the input author."
  (when (not (string= "" author))
    (add-to-list 'magit-commit-arguments (concat "--author=" author))
    (minibuffer-message (concat "Author overridden with '" author "'"))))

(defun my/git-override-author ()
  "Activates a git commit author override using the input author."
  (interactive)
  (let ((author (read-string "i.e., A U Thor <author@example.com>: " my/magit-gc-override-author)))
    (setq my/magit-gc-override-author author)
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
  (interactive)
  (if (find-if (lambda (s) (string-match "--author" s)) magit-commit-arguments)
    (my/git-remove-author-override)
    (my/git-set-author my/magit-gc-override-author)))

(use-package! git-link
  :bind (("C-c C-g l" . git-link)))

;; (use-package magit
;;   :bind (("C-c C-p" . my/git-override-author)
;;         ("C-c C-u" . my/git-remove-author-override)
;;         ("C-c C-t" . my/git-author-toggle)))
