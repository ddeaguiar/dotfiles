#
# Executes commands at the start of an interactive session.
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Shell options
setopt append_history
setopt extended_history
setopt inc_append_history
setopt hist_expire_dups_first
setopt hist_ignore_space
setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt hist_reduce_blanks
setopt hist_verify
setopt share_history
setopt rm_star_wait
setopt auto_pushd
setopt pushd_ignore_dups
setopt long_list_jobs
unsetopt auto_name_dirs

# Source scripts for specific tasks
alias history="fc -l 1"
source $HOME/.aliases
if [ -d $HOME/.ec2 ]; then
  if [ -f $HOME/.ec2/.ec2rc ]; then
    source $HOME/.ec2/.ec2rc
  fi
fi
source $HOME/.passwd
source $HOME/.javarc
source $HOME/.oraclerc
source $HOME/.path

# Additional completions
source $HOME/.oh-my-zsh/plugins/git-flow/git-flow.plugin.zsh

# Racket completions
source $HOME/lib/racket/racket-completions.zsh

MTSSHKEYS="$HOME/.ssh/ec2-mtc.pem"

# Helper function that prepends the Amazon Linux AMI
# user to a server name if it contains 'ec2'
_mtprependuser() {
  if [[ "$1" == *ec2* ]]; then
    echo "ec2-user@$1"
  else
    echo "$1"
  fi
}

mtssh() {
  local server=$(_mtprependuser "$1")

  if [ $# -gt 1 ]; then
     local rest="\"${@:2}\""
  fi

  ssh -i $MTSSHKEYS $server $rest
}


mtscp() {
  local source=$(_mtprependuser "$1")
  local target=$(_mtprependuser "$2")

  scp -i $MTSSHKEYS "$source" "$target"
}


# AWS
source /usr/local/bin/aws_zsh_completer.sh

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

### rbenv support
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
