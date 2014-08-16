(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(install-packs '(clojure-cheatsheet
                 google-this
                 floobits))


;; Load bindings config
(live-load-config-file "bindings.el")
(live-load-config-file "lusty-explorer.el")
(live-load-config-file "lorem-ipsum.el")
(live-load-config-file "clean-mode-line.el")
(live-load-config-file "dda-org-mode.el")
(live-load-config-file "compojure-indent.el")
(live-load-config-file "tmux.el")
(live-load-config-file "ddeaguiar-conf.el")

;;(live-load-config-file "clojure-test-mode.el")

;; Load libraries
(live-add-pack-lib "midje-mode")
(require 'midje-mode)
;;(require 'clojure-jump-to-file)

(live-add-pack-lib "geiser")
(require 'geiser)

(add-hook 'clojure-mode-hook 'midje-mode)
