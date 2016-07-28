testing
testFromSmallFile
	| group |
	group := HistoryVersionGroup newFromDirectory: (HistoryTestTools file: '5', FileDirectory slash, '5').
	self assert: 1 equals: group versionsByName size.
	self assert: #doIt equals: (((group versionsByName at: '5.0') changesByName at: 'Squeak5.0Short.sources') first type).
	self assert: 18 equals: group changes size.
	self assert: 9 equals: (group contributorsBag occurrencesOf: 'Andreas Raab')