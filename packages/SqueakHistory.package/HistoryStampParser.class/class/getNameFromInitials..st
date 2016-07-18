parsing
getNameFromInitials: anInitial
	^ (self nameByInitials includesKey: anInitial) 
		ifTrue: [self nameByInitials at: anInitial]
		ifFalse: [anInitial]
	