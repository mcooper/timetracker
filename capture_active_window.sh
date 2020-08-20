#! /bin/bash

#Run with the following cron line:
#* * * * * DISPLAY=:0 /home/mattcoop/timetracker/capture_active_window.sh

#If idle for less than 30000 milliseconds, record the active window
if [[ $(xprintidle) -lt 30000 ]]
  then
    date=$(date -d '- 4 hours' +%Y-%m-%d)
    time=$(date +%H:%M:%S)
    activewindow=$(xdotool getactivewindow getwindowname)
    touch /home/mattcoop/timetracker/log/$date
    echo -e "$time\t$activewindow" >> /home/mattcoop/timetracker/log/$date
fi
