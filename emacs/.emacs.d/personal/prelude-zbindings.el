;;Fix keybindings under iTerm2
(global-set-key "\e[1;9A" [M-up])
(global-set-key "\e[1;9B" [M-down])
(global-set-key "\e[1;9C" [M-right])
(global-set-key "\e[1;9D" [M-left])

(global-set-key (kbd "C-c <C-left>") 'windmove-left)
(global-set-key (kbd "C-c <C-right>") 'windmove-right)
(global-set-key (kbd "C-c <C-up>") 'windmove-up)
(global-set-key (kbd "C-c <C-down>") 'windmove-down)

(global-set-key (kbd "C-c M-/") 'comment-region)
(global-set-key (kbd "s-<backspace>") 'backward-kill-word)

;; Utilities
(global-set-key (kbd "C-c =") 'prelude-increment-integer-at-point)
(global-set-key (kbd "C-c _") 'prelude-decrement-integer-at-point)

;; Ctrl-x r i Useful rectangle binding
(global-set-key (kbd "C-x r i") 'string-insert-rectangle)

;; Buffer shortcuts
(global-set-key (kbd "C-x p") 'print-buffer)

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
(global-set-key (kbd "C-c w s") 'swap-windows)
(global-set-key (kbd "C-c w r") 'rotate-windows)
(global-set-key (kbd "C-c w p") 'buf-move-up)
(global-set-key (kbd "C-c w n") 'buf-move-down)
(global-set-key (kbd "C-c w b") 'buf-move-left)
(global-set-key (kbd "C-c w f") 'buf-move-right)
(global-set-key (kbd "C-c w -") 'shrink-window)
(global-set-key (kbd "C-c w =") 'enlarge-window)
(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "M-y") 'browse-kill-ring)
(global-set-key (kbd "M-p") 'fill-paragraph)
(define-key prelude-mode-map (kbd "M-o") 'other-window)

;; Frame Management
(global-set-key (kbd "s-o") 'other-frame)
(define-key prelude-mode-map (kbd "s-o") 'other-frame)

(use-package smartparens
  :diminish (smartparens-mode . " (Sm)")
  :init
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
   ("H-t"              . sp-prefix-tag-object)
   ("H-p"              . sp-prefix-pair-object)
   ("H-s c"            . sp-convolute-sexp)
   ("H-s a"            . sp-absorb-sexp)
   ("H-s e"            . sp-emit-sexp)
   ("H-s p"            . sp-add-to-previous-sexp)
   ("H-s n"            . sp-add-to-next-sexp)
   ("H-s j"            . sp-join-sexp)
   ("H-s s"            . sp-split-sexp)
   :map emacs-lisp-mode-map
   (")"                . sp-up-sexp))
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'web-mode "<" nil :when '(personal/sp-web-mode-is-code-context))
  (sp-with-modes '(html-mode sgml-mode)
    (sp-local-pair "<" ">")))

;; hydra
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(defhydra hydra-splitter (global-map "C-M-s")
  "splitter"
  ("h" hydra-move-splitter-left)
  ("j" hydra-move-splitter-down)
  ("k" hydra-move-splitter-up)
  ("l" hydra-move-splitter-right))

(defhydra hydra-apropos (:color blue
                                :hint nil)
  "
_a_propos        _c_ommand
_d_ocumentation  _l_ibrary
_v_ariable       _u_ser-option
^ ^          valu_e_"
  ("a" apropos)
  ("d" apropos-documentation)
  ("v" apropos-variable)
  ("c" apropos-command)
  ("l" apropos-library)
  ("u" apropos-user-option)
  ("e" apropos-value))
;; Recommended binding:
(global-set-key (kbd "C-c H") 'hydra-apropos/body)

(provide 'personal/prelude-zbindings)
