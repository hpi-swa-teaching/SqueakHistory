as yet unclassified
getTimeStampFromDateString: string
	"Answer a TimeStamp that corresponds to my (text) stamp"
	| tokens |
	tokens := string findTokens: Character separators.
	^ tokens size = 2
		ifTrue: [[| date time |
			date := Date fromString: (tokens at: tokens size - 1).
			time := Time fromString: tokens last.
			TimeStamp date: date time: time]
				on: Error
				do: [:ex | ex
						return: (TimeStamp fromSeconds: 0)]]
		ifFalse: [TimeStamp fromSeconds: 0]