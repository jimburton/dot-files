#!/usr/bin/env bash

# Fetch workspace list from Niri
WORKSPACES=$(niri msg --json workspaces 2>/dev/null)

if [ -z "$WORKSPACES" ] || [ "$WORKSPACES" = "[]" ]; then
    echo '{"text":"Niri Offline"}'
    exit 0
fi

# Use standard Pango attributes (foreground, background, weight)
echo "$WORKSPACES" | jq -c '
  sort_by(.idx) 
  | map(
      if .is_focused then
        "<span foreground=\"#ffffff\" background=\"#89b4fa\" weight=\"bold\"> " + (.idx | tostring) + " </span>"
      elif .is_urgent then
        "<span foreground=\"#ffffff\" background=\"#f38ba8\" weight=\"bold\"> " + (.idx | tostring) + " </span>"
      else
        "<span foreground=\"#a6adc8\"> " + (.idx | tostring) + " </span>"
      end
    ) 
  | join(" ") 
  | {text: .}
'
