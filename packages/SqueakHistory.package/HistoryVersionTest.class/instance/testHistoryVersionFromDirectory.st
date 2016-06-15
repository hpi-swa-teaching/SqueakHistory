as yet unclassified
testHistoryVersionFromDirectory
	| version |
	version := HistoryVersion newFromDirectory: (TestTools file: '1.23').
	self assert: '1.23' equals: version name.
	self assert: 5 equals: version changesByName size.
	self assert: #doIt equals: ((version changesByName at: 'Squeak1.23.changes') changeList first type)