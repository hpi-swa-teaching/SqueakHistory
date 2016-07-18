parsing
getNameFromStamp: aStamp
	^self getNameFromInitials: (self getInitialsFromStamp: aStamp)