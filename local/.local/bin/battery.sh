#!/usr/bin/env python3

import subprocess
import time
import psutil

battery = psutil.sensors_battery()
plugged = battery.power_plugged
percent = str(int((battery.percent)))
if plugged == False: plugged = "Not Plugged In"
else: plugged = "Plugged In"

print(percent + '%')
