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

# Ensure that a non-login, non-interactive shell has a defined environment.
if [[ "$SHLVL" -eq 1 && ! -o LOGIN && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi

#Boot2docker
export DOCKER_HOST=tcp://192.168.59.103:2376
export DOCKER_CERT_PATH=/Users/ddeaguiar/.boot2docker/certs/boot2docker-vm
export DOCKER_TLS_VERIFY=1

#Boot
export BOOT_JVM_OPTIONS="-client -XX:+TieredCompilation -XX:TieredStopAtLevel=1 -Xmx2g -XX:MaxPermSize=256m -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -Xverify:none"
