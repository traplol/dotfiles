#!/bin/bash
i3status -c $1 | while :
do
    read line
    to_be_fixed=$(echo $line | sed 's/\[{/{/')
    MEM_USAGE=$(awk '/MemTotal/ {memtot=$2}; /MemAvailable/ {memavail=$2}; END { printf("%00.0f%%", (1-memavail/memtot)*100) }' /proc/meminfo)
JSON=$(cat <<EOF
{"name":"mem_usage","markup":"none","full_text":"[Mem:$MEM_USAGE]"}
EOF
)
    if [[ $line == "[{"* ]] ; then
        echo "[$JSON,$to_be_fixed," || exit 1
    elif [[ $line == ",[{"* ]] ; then
        echo "[$JSON$to_be_fixed," || exit 1
    else
        echo $line || exit 1
    fi
done
