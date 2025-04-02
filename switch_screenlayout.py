#!/usr/bin/python

import pathlib
import subprocess

SCREENLAYOUT_DIR = pathlib.Path.home() / '.screenlayout'

xrandr_output = subprocess.check_output(['xrandr', '-q'])
num_active_displays = xrandr_output.count(b'*')
if 1 == num_active_displays:
    subprocess.run(SCREENLAYOUT_DIR / 'nytk.sh')
elif 2 == num_active_displays:
    subprocess.run(SCREENLAYOUT_DIR / 'default.sh')
else:
    # more than two active displays, this is unexpected
    # guess I'll just go back to the one display layout
    subprocess.run(SCREENLAYOUT_DIR / 'nytk.sh')
