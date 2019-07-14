# Executes commands at login pre-zshrc.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

#
# Paths
#

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

# Set the the list of directories that cd searches.
# cdpath=(
#   $cdpath
# )

# Set the list of directories that Zsh searches for programs.
path=(
  /bin
  $HOME/Library/Python/3.7/bin
  $HOME/.rbenv/bin:$PATH
  $HOME/bin
  /usr/local/{bin,sbin}
  /opt/homebrew-cask/Caskroom/racket/6.1/Racket\ v6.1/bin
  $HOME/datomic-pro/bin
  $HOME/jbake/bin
  $HOME/.jenv/bin
  $PATH
)

TMPPREFIX="${TMPDIR%/}/zsh"
if [[ ! -d "$TMPPREFIX" ]]; then
  mkdir -p "$TMPPREFIX"
fi

# DIRENV
eval "$(direnv hook $SHELL)"
