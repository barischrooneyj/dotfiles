#!/usr/bin/osascript

tell application "System Preferences"
	activate
	set current pane to pane "com.apple.preference.energysaver"
end tell
delay 1

tell application "System Events"
	tell process "System Preferences"
		tell window 1
			tell checkbox 0
				if not (its value as boolean) then click
			end tell
		end tell
	end tell
end tell

tell application "System Preferences"
	quit
end tell
