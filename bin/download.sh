#!/usr/bin/zsh

s() {
  if [ $1 -ge 1048576 ]; then
    echo `awk "BEGIN{printf(\"%.2f\",$1/1048576)}"`\ MB/s
  elif [ $1 -ge 1024 ]; then
    echo `awk "BEGIN{printf(\"%.2f\",$1/1024)}"`\ KB/s
  else
    echo $1 B/s
  fi
}

preRX=`cat /etc/download`

RX=`echo \`ip -s link ls ens33 | sed -n '4p'\` | cut -d ' ' -f 1`

sudo zsh -c "echo $RX > /etc/download"

echo `s $[RX-preRX]`
