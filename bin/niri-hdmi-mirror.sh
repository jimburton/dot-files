#!/usr/bin/env bash

# Query outputs via niri's built-in IPC
OUTPUTS_JSON=$(niri msg --json outputs)

# Find primary/internal display (eDP)
PRIMARY_OUTPUT=$(echo "$OUTPUTS_JSON" | jq -r 'keys[] | select(startswith("eDP"))' | head -n 1)

# Find connected external HDMI output
HDMI_OUTPUT=$(echo "$OUTPUTS_JSON" | jq -r 'keys[] | select(startswith("HDMI"))' | head -n 1)

# Fallback if eDP display isn't detected
if [ -z "$PRIMARY_OUTPUT" ]; then
    PRIMARY_OUTPUT=$(echo "$OUTPUTS_JSON" | jq -r 'keys[]' | head -n 1)
fi

# Manage wl-mirror state
if [ -n "$HDMI_OUTPUT" ]; then
    if ! pgrep -x "wl-mirror" > /dev/null; then
        wl-mirror --fullscreen-output "$HDMI_OUTPUT" "$PRIMARY_OUTPUT" &
    fi
else
    pkill -x wl-mirror
fi
