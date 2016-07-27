testing
testHistoryVersionGroupFromDirectory
	| group |
	group := HistoryVersionGroup newFromDirectory: (HistoryTestTools file: '1', FileDirectory slash, '1.16 - 1.3').
	self assert: 2 equals: group versionsByName size.
	self assert: #doIt equals: (((group versionsByName at: '1.16') changesByName at: 'Squeak1.16u.changes') first type)