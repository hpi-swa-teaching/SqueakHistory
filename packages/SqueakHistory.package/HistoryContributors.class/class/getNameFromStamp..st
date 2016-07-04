as yet unclassified
getNameFromStamp: aStamp
	"comment stating purpose of message"

	^self getNameFromInitials: (self getInitialsFromStamp: aStamp)