#
# Defines environment variables.

export CASE_SENSITIVE="true"
export DISABLE_AUTO_UPDATE="false"
export RUBYOPT="rubygems"
export ARCHFLAGS="-arch x86_64"
export GREP_OPTIONS="--color=auto"
export GREP_COLOR="7;33"
export EDITOR="vim"
export CLICOLOR="yes"
export REPORTTIME=30
export LESS="--ignore-case --LONG-PROMPT --QUIET --chop-long-lines -Sm --RAW-CONTROL-CHARS --quit-if-one-screen --no-init"
export PAGER=less
export LC_CTYPE=en_US.UTF-8
export HISTIGNORE="&:exit:reset:clear"
export HISTSIZE=25000
export HISTFILE=~/.zsh_history
export SAVEHIST=10000
# Allow emacsclient to fire up an emacs daemon if
# one is not already running.
export ALTERNATE_EDITOR=""
export BUKKIT_PLUGINS_DIR="$HOME/bukkit/plugins"
export EC2_HOME="$HOME/ec2-api-tools"
export PLTCOLLECTS=":$HOME/Library/Racket/collections"
export CS_HOME="$HOME/bin/cloud-search"
export MONO_GAC_PREFIX="/usr/local"

#https://github.com/sorin-ionescu/prezto/tree/master/modules/python
export VIRTUALENVWRAPPER_VIRTUALENV_ARGS='--no-site-packages'
export PROJECT_HOME="$HOME/src/ps"

#Java
export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Home
export ANT_OPTS="-Xms512M -Xmx512M -XX:+UseParallelGC"
export JRE_HOME=$JAVA_HOME

#Oracle
# export ORACLE_HOME=/opt/local/oracle/instantclient_10_2
# export CLASSPATH=$ORACLE_HOME:$ORACLE_HOME/ojdbc14.jar:$CLASSPATH
# export DYLD_LIBRARY_PATH=$ORACLE_HOME
# export LD_LIBRARY_PATH=$ORACLE_HOME
# export SQLPATH=$ORACLE_HOME
# export TNS_ADMIN=/opt/local/oracle/network/admin
# export NLS_LANG=AMERICAN_AMERICA.UTF8
# export PATH=$PATH:$DYLD_LIBRARY_PATH:$SQLPATH
# export RC_ARCHS=x86_64

# Ensure that a non-login, non-interactive shell has a defined environment.
if [[ "$SHLVL" -eq 1 && ! -o LOGIN && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi

#Boot2docker
export DOCKER_HOST=tcp://192.168.59.103:2376
export DOCKER_CERT_PATH=/Users/ddeaguiar/.boot2docker/certs/boot2docker-vm
export DOCKER_TLS_VERIFY=1

#Boot
export BOOT_JVM_OPTIONS="-client -XX:+TieredCompilation -XX:TieredStopAtLevel=1 -Xmx2g -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -Xverify:none"

# go
export GOPATH=$HOME/gocode

# add my bin dir to path
export PATH="$HOME/bin:$PATH"

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

## So rbenv and tmux play nice
## See https://github.com/sstephenson/rbenv/issues/369
# Mac OS X uses path_helper to preload PATH, clear it out first
if [ -x /usr/libexec/path_helper ]; then
    PATH=''
    eval `/usr/libexec/path_helper -s`
fi
