#!/bin/bash
set -e

_usage() {
    prog=$(basename $0)
    echo "usage: $prog <user> <group> <server>"
    echo 
    exit 1
}

test $# -eq 3 || _usage 

user=$1
group=$2
server=$3

ssh -t -o ConnectTimeout=10 $user@$server "sudo mkdir -p /home/$user && sudo chown $user.$group /home/$user && sudo chmod 700 /home/$user"

ssh-copy-id $user@$server
