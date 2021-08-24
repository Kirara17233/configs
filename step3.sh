#!/usr/bin/zsh

user=#user

curl -o /etc/ssh/.ssh/id_rsa "https://raw.githubusercontent.com/Kirara17233/rsa/main/id_rsa?token=$1"
ln -s /etc/ssh/.ssh/id_rsa /etc/skel/.ssh/id_rsa
ln -s /etc/ssh/.ssh/id_rsa /home/$user/.ssh/id_rsa
rm /step3.sh
