as yet unclassified
testHistoryVersion
	| changes version |
	changes := HistoryChangesParser parseChangesFrom: (HistoryTestTools file: '1.23', FileDirectory slash, 'Squeak1.23.changes').
	version := HistoryVersion newFromName: '1.23' changesByName: (Dictionary newFromPairs: {'Squeak1.23.changes'. changes}).
	self assert: '1.23' equals: version name.
	self assert: 1 equals: version changesByName size.
	self assert: #doIt equals: ((version changesByName at: 'Squeak1.23.changes') first type).
	HistoryChangesBrowser showChanges: changes