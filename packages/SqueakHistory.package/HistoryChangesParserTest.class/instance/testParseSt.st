testing
testParseSt
	| changes |
	changes := HistoryChangesParser parseChangesFrom: (HistoryTestTools file: 'source.st').

	self assert: 2389 equals: (changes size).
	HistoryChangesBrowser showChanges: changes