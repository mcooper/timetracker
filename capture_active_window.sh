#! /bin/bash

#Run with the following cron line:
#* * * * * DISPLAY=:0 /home/mattcoop/timetracker/capture_active_window.sh

#See if mouse is moving over the course of the minute
mouse_start=$(xdotool getmouselocation)
sleep 55s
mouse_end=$(xdotool getmouselocation)

#If it is moving, record the active window
if [[ "$mouse_start" != "$mouse_end" ]]
  then
    date=$(date +%Y-%m-%d)
    time=$(date +%H:%M:%S)
    activewindow=$(xdotool getactivewindow getwindowname)
    touch /home/mattcoop/timetracker/log/$date
    echo -e "$time\t$activewindow" >> /home/mattcoop/timetracker/log/$date
fi
