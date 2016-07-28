testing
test
	| types changes |
	changes :=HistoryChangesParser parseChangesFrom: (HistoryTestTools file: 'source.st').
	HistoryChangesParser parseChangesFrom: (HistoryTestTools file: 'Squeak1.1.changes').

	types := (changes groupBy: [:it | it type]) keys asSet.
	self assert: #(#classComment #doIt #method) asSet equals: types
	