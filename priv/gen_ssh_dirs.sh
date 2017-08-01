#!/bin/sh
cd $(dirname $0)
mkdir -p ssh_daemon ; ssh-keygen -t rsa -f ssh_daemon/ssh_host_rsa_key
mkdir -p ssh_user/.ssh ; ssh-keygen -t rsa -f ssh_user/.ssh/id_rsa
cp ssh_user/.ssh/id_rsa.pub ssh_user/.ssh/authorized_keys


