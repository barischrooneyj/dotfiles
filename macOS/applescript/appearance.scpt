tell application "System Events"
	tell appearance preferences
		set properties to {highlight color:purple, dark mode:true}
	end tell
end tell

tell application "System Preferences"
	reveal anchor "Main" of pane id "com.apple.preference.general"
end tell

tell application "System Events"
	repeat until exists of window "General" of application process "System Preferences"
		delay 0.1
	end repeat
	click checkbox "Purple" of window "General" of application process "System Preferences"
end tell
tell application "System Preferences" to quit
