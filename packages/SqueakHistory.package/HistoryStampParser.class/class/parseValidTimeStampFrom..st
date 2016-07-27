parsing
parseValidTimeStampFrom: tokens
	| date time |
	^ [date := Date fromString: (tokens at: tokens size - 1).
	time := Time fromString: tokens last.
	TimeStamp date: date time: time]
		on: Error
		do: [:ex | ex return: (TimeStamp fromSeconds: 0)]