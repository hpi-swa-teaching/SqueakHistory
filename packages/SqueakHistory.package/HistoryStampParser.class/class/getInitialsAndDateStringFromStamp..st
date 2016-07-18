parsing
getInitialsAndDateStringFromStamp: stamp 
	| initials date |
	initials := ''. 
	date := ''.
	(stamp isNil or: [stamp isEmpty]) ifTrue: [^ {''. ''}].
	(stamp subStrings: (' ', Character cr, Character tab))
		do: [:sub |
			(self isDatePart: sub)
				ifTrue: [date := (date ifEmpty: [''] ifNotEmpty: [date , ' ']), sub]
				ifFalse: [initials := sub]].
	^ {initials . date}