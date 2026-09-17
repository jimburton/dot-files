#!/usr/bin/env bash

set -euo pipefail

# Expecting one argument, 'l' or 'r'
direction="${1:-}"
case "$direction" in
    l) action="focus-column-left" ;;
    r) action="focus-column-right" ;;
    *)
        notify-send "Focus error!" "Missing direction. Must be 'l' or 'r'."
        exit 1
        ;;
esac

CONFIG_FILE="$HOME/.config/niri/config.kdl"

# Reset active-color if something goes wrong
cleanup() {
    sed -i 's/active-color "#ff0000"/active-color "#7fc8ff"/g' "$CONFIG_FILE"
    niri msg action reload-config >/dev/null 2>&1 || true
}
trap cleanup EXIT

prev_id=$(niri msg -j focused-window | jq -r '.id')
niri msg action "$action"
new_id=$(niri msg -j focused-window | jq -r '.id')

# If we were already at the left or right edge of the row then flash the window border,
# temporarily unmaximising if necessary to show the border.
if [[ "$new_id" == "$prev_id" ]]; then
    # Determine if the window is currently maximised by comparing window width to screen width
    window_width=$(niri msg -j focused-window | jq -r '.layout.window_size[0]')
    screen_width=$(niri msg -j outputs | jq -r '.[].logical.width // empty' | head -n1)

    if [[ "$window_width" -eq "$screen_width" ]]; then
	is_max="true"
    else
	is_max="false"
    fi

    if [[ "$is_max" == "true" ]]; then
        # Un-maximize to reveal the border
        niri msg action maximize-column
    fi

    # Flash red
    sed -i 's/active-color "#7fc8ff"/active-color "#ff0000"/g' "$CONFIG_FILE"
    niri msg action load-config-file

    # sleep to show the border
    sleep 0.15

    # Revert to blue
    sed -i 's/active-color "#ff0000"/active-color "#7fc8ff"/g' "$CONFIG_FILE"
    # force IO to make sure niri notices the change
    sync "$CONFIG_FILE"
    niri msg action load-config-file

    if [[ "$is_max" == "true" ]]; then
        niri msg action maximize-window-to-edges
    fi
fi
