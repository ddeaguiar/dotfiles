#
# Executes commands at the start of an interactive session.
#
fpath=($HOME/lib/zsh $fpath)
#export FPATH="$HOME/lib/zsh:$FPATH"

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Shell options
setopt rm_star_wait
setopt auto_pushd
setopt pushd_ignore_dups
setopt long_list_jobs
unsetopt auto_name_dirs

# Source scripts for specific tasks
alias history="fc -l 1"
source $HOME/.aliases

# aws autocompletion file needs to be sourced
source /usr/local/bin/aws_zsh_completer.sh

### rbenv support
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
