set -u fish_greeting
set EDITOR emacs

set PATH $HOME/datomic-cli $HOME/bin $HOME/datomic-pro/bin $HOME/.jenv/bin $HOME/.emacs.d/bin $PATH

status --is-interactive; and source (jenv  init -|psub)

alias rf="rm -rf"
alias lrb="lein with-profiles +rebl,+socket-rebl,+socket-prepl repl"
alias lsb="lein with-profiles +socket,+socket-prepl repl"

eval (direnv hook fish)

starship init fish | source
