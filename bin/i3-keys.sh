#!/bin/env bash

grep "^[ ]*bindsym" ~/.config/i3/config | sed -e 's/^[ ]*bindsym //' -e 's/^[ ]*$mod/win/' | less
