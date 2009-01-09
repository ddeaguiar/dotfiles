# Include aliases, completion, prompt, and app-specific settings
if [ -f $HOME/.passwd ]; then
  source $HOME/.passwd
fi

if [ -d $HOME/.ec2 ]; then
  if [ -f $HOME/.ec2/ec2.bash ]; then
    source $HOME/.ec2/ec2.bash
  fi
fi  

if [ -f $HOME/.bash_aliases_mac ]; then
    source $HOME/.bash_aliases_mac
fi 

source $HOME/.path
source $HOME/.bash_aliases
source $HOME/.bash_completion/*
source $HOME/.javarc
source $HOME/.oraclerc
source $HOME/bin/vcs_prompt

#source $HOME/.bashrc

# Grep
export GREP_OPTIONS='--color=auto' 
export GREP_COLOR='6;35'

# *NIX Editors
export EDITOR="vi"
export GIT_EDITOR="mate -w"
export SVN_EDITOR="mate -w"

# Evented Mongrel
export EVENT=1

# Bash History Control
export HISTCONTROL=erasedups
export HISTSIZE=5000
shopt -s histappend
PROMPT_COMMAND="history -a;$PROMPT_COMMAND"

export LC_CTYPE=en_US.UTF-8


# Misc Bash Functions
function clip() {
  cat $1 | pbcopy
}

function printfile() {
  cat $1 | lpr
}

function go() {
  open -a $1.app
}

# Usage: git-doc-up 1.6.1
function git-doc-up() {
	wget http://www.kernel.org/pub/software/scm/git/git-manpages-$1.tar.bz2
	sudo tar xjv -C /usr/share/man -f git-manpages-$1.tar.bz2
	rm git-manpages*bz2
	echo "Be sure to run 'sudo periodic weekly'"
}
