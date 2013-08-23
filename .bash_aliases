alias cls=clear
alias showhosts="cat /etc/hosts"
alias ll="ls -lah"
alias ss="./script/server start"
alias sc="./script/console"
alias as="autospec"
alias lsip="ipconfig | ruby -e 'puts ARGF.read.scan(/\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}/).sort.uniq.reject {|x| x.include?(\"255\")'}"
alias ipconfig=ifconfig
alias git-serve="git daemon --export-all --base-path=/Users/ddeaguiar/Sites/git --detach"
alias git-rollback="git reset --hard HEAD"
alias catless="cat $1 | less"
alias ..="cd .."
alias ...="cd ../.."
alias realias="source ~/.bash_aliases & ~/.bash_aliases_mac"
alias log="git log --color"
alias truncate="cp /dev/null"
alias m='mate .'
alias actl="sudo apachectl -k restart"
alias rs='rake spec RAILS_ENV=test'
alias git-add-rm='git add . && git st | grep [d]eleted | awk '\''{print $3}'\'' | xargs git rm'
alias deploy="rake deploy"
alias flushdns="dscacheutil -flushcache"
alias lll="ls -la | less"
alias myhosts="dscl localhost -list /Local/Default/Hosts"
alias rf="rm -rf"
alias lshosts="cat /etc/hosts"
alias v="vim ."
alias restart_syslog="launchctl unload /System/Library/LaunchDaemons/com.apple.syslogd.plist; sleep 1; launchctl load /System/Library/LaunchDaemons/com.apple.syslogd.plist"
alias rfeat='rake features RAILS_ENV=test'
alias scum='export RAILS_ENV=cucumber && script/cucumber'
alias ass="export RAILS_ENV=test && script/autospec"
alias at="export RAILS_ENV=test && rake"
alias ocroute="sudo route -n add -net 10.5.0.0/16 -interface ppp0"
alias onroute="sudo route -n add -net 192.168.200 -interface ppp0"
alias osroute="sudo route -n add -net 192.168.170 -interface ppp0"
#alias croute="sudo route -n add -net 10.5.0.0/16 -interface jnc0"
#alias nroute="sudo route -n add -net 192.168.200 -interface jnc0"
alias cf='compass compile -q --force && compass watch -q'
alias cw='compass watch -q'
alias scrum="git log --oneline --author="ddeaguiar" HEAD@{yesterday}..HEAD"
alias rnr="rlwrap node-repl"
alias rio="rlwrap io"
alias dcc="drush cc all -y"
alias mdb="mongod run --config /usr/local/Cellar/mongodb/1.6.2-x86_64/mongod.conf"
alias untar="tar xvzf"

alias pa="git apply --whitespace=fix patches/*.patch"
alias rpa="git apply -R patches/*"

alias pdstart="sudo pound -v -f /usr/local/etc/pound.cfg"
alias pdstop="sudo killall -HUP pound"
alias guard="nocorrect guard"
alias redis-s="redis-server /usr/local/etc/redis.conf"
alias datomic-free="~/.datomic-free/bin/datomic-free"

alias es='emacs --daemon'
alias em='emacsclient -c'
alias ek='emacsclient -e "(kill-emacs)"'
alias edc="es && em"

alias tx="tmuxinator"
