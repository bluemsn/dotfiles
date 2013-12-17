#!/bin/sh
while true; do
	BATTERY=`echo -n "{i 14} " && acpi | grep -o '[0-9]*%'`
	CLOCK=`echo -n "{i 0} " && date +'%F %R'`

	echo "$BATTERY $CLOCK"
	sleep 60
done
