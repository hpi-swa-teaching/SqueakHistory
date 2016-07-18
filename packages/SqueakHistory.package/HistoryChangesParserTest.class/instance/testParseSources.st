testing
testParseSources
	| changeList |
	changeList := HistoryChangesParser parseChangesFrom: (HistoryTestTools file: 'SqueakV50Short.sources').
	self assert: 18 equals: (changeList size)