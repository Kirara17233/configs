#!/usr/bin/zsh

s() {
  if [ $1 -ge 1048576 ]; then
    echo `awk "BEGIN{printf(\"%.1f\",$1/1048576)}"`MB/s
  elif [ $1 -ge 1024 ]; then
    echo `awk "BEGIN{printf(\"%.1f\",$1/1024)}"`KB/s
  else
    echo $(($1))B/s
  fi
}

preTX=`cat /etc/upload`

TX=`echo \`ip -s link ls ens33 | sed -n '6p'\` | cut -d ' ' -f 1`

sudo zsh -c "echo $TX > /etc/upload"

echo `s $[TX-preTX]`
