#!/usr/bin/osascript
tell application "Safari"
	make new document with properties {URL:"http://wiki.home.moimoitei.jp/farm/techmemo"}
	set idx to (count every window)
	set theWindow to window idx
	set bounds of theWindow to {672, 22, 1440, 840}
	set theID to id of theWindow
	set cmdStr to "~/common/bin/cgsutils -r " & theID & " -t 5"
	do shell script cmdStr
end tell

