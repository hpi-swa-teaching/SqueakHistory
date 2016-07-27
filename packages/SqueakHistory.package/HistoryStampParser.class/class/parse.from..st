parsing
parse: initialsAndDate from: stampSubstring
	| initials date |
	initials := initialsAndDate first.
	date := initialsAndDate second.
	^ (self isDatePart: stampSubstring)
		ifTrue: [{initials . (date
						ifEmpty: ['']
						ifNotEmpty: [date , ' ']), stampSubstring}]
		ifFalse: [{stampSubstring . date}]