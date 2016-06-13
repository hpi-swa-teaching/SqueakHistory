as yet unclassified
testHistoryVersion
	|changeList version|
	changeList := SourceParser parseChangeListFrom: (TestTools file: '1.23', FileDirectory slash, 'Squeak1.23.changes').
	version := HistoryVersion newFromName: '1.23' changesByName: (Dictionary newFromPairs: {'Squeak1.23.changes'. changeList}).
	self assert: '1.23' equals: version name.
	self assert: 1 equals: version changesByName size.
	self assert: #doIt equals: ((version changesByName at: 'Squeak1.23.changes') changeList first type).