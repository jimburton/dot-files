#!/usr/bin/env bash

STAT=`pulseaudio-ctl full-status`

VOL=`echo $STAT | cut -d " " -f 1`
SINK_MUTE=`echo $STAT | cut -d " " -f 2`
SRC_MUTE=`echo $STAT | cut -d " " -f 3`

if [ "$SINK_MUTE" = "yes" ]; then
    OUT="mute"
else
    OUT="$VOL"
fi
   
echo "Vol: $OUT"

exit 0
