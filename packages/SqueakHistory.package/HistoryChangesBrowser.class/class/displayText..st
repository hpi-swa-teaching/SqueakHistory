display text
displayText: changeRecord
	^ ((self displayTextByType at: changeRecord type ifAbsent: [self halt "todo: implement this type in displayTextByType"]) value: changeRecord), 
	(self authorDisplayText: changeRecord), (self dateDisplayText: changeRecord)