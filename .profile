# Include aliases, completion, prompt, and app-specific settings
if [ -s ~/.rvm/scripts/rvm ] ; then 
	source ~/.rvm/scripts/rvm ;
fi

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

source $HOME/.exports
source $HOME/.path
source $HOME/.bash_aliases
source $HOME/.bash_completion/*
source $HOME/.javarc
source $HOME/.oraclerc
source $HOME/bin/vcs_prompt

# Bash History Control
shopt -s histappend
PROMPT_COMMAND="history -a;$PROMPT_COMMAND"

# Misc Bash Functions

function parse_git_branch() {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

function proml {
	local        BLUE="\[\033[0;34m\]"
	local         RED="\[\033[0;31m\]"
	local   LIGHT_RED="\[\033[1;31m\]"
	local       GREEN="\[\033[0;32m\]"
	local LIGHT_GREEN="\[\033[1;32m\]"
	local       WHITE="\[\033[1;37m\]"
	local  LIGHT_GRAY="\[\033[0;37m\]"
	local      YELLOW="\[\033[0;33m\]"
	local  LIGHT_BLUE="\[\033[0;36m\]"
	case $TERM in
		xterm*)
		#TITLEBAR='\[\033]0;\u@\h:\w\007\]'
		TITLEBAR=''
		;;
		*)
		TITLEBAR=""
		;;
	esac

PS1="${TITLEBAR}\
$LIGHT_BLUE\w $RED\$(parse_git_branch)\
$GREEN > $YELLOW"
PS2='> '
PS4='+ '
}


function encrypt() {
	openssl des3 -salt -in "$1" -out "$2"
}

function decrypt() {
	openssl des3 -d -salt -in "$1" -out "$2"
}

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

function sw-ec2() {
	rm .ec2
	ln -s $1 .ec2
	source .profile
	echo "ec2 link switched to $1"
}

# Set prompt
proml

# -- start rip config -- #
RIPDIR=/Users/ddeaguiar/.rip
RUBYLIB="$RUBYLIB:$RIPDIR/active/lib"
PATH="$PATH:$RIPDIR/active/bin"
export RIPDIR RUBYLIB PATH
# -- end rip config -- #
if [[ -s /Users/ddeaguiar/.rvm/scripts/rvm ]] ; then source /Users/ddeaguiar/.rvm/scripts/rvm ; fi
