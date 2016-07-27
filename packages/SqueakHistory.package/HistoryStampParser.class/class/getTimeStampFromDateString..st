parsing
getTimeStampFromDateString: aString
	"Answer a TimeStamp that corresponds to my (text) stamp"
	| tokens |
	tokens := aString findTokens: Character separators.
	^ tokens size = 2
		ifTrue: [self parseValidTimeStampFrom: tokens]
		ifFalse: [TimeStamp fromSeconds: 0]