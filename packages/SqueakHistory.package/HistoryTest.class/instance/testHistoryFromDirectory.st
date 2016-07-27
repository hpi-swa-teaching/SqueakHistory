testing
testHistoryFromDirectory
	| history |
	history := History newFromDirectory: (HistoryTestTools file: '1').
	self assert: 2 equals: history versionGroupsByName size.
	self assert: #doIt equals: ((((history versionGroupsByName at: '1.16 - 1.3') 
		versionsByName at: '1.16') changesByName at: 'Squeak1.16u.changes') first type)