set -u fish_greeting
set EDITOR emacs

set PATH $HOME/datomic-cli $HOME/bin $HOME/datomic-pro/bin $HOME/.jenv/bin $HOME/.emacs.d/bin $PATH
set -x RUBY_CONFIGURE_OPTS "--with-openssl-dir="(brew --prefix openssl@1.1)

status --is-interactive; and source (jenv  init -|psub)

if test -f $HOME/.nurc
   bass source $HOME/.nurc
end

alias rf="rm -rf"
alias lrb="lein with-profiles +rebl,+socket-rebl,+socket-prepl repl"
alias lsb="lein with-profiles +socket,+socket-prepl repl"

eval (direnv hook fish)

status --is-interactive; and source (rbenv init -|psub)

starship init fish | source
