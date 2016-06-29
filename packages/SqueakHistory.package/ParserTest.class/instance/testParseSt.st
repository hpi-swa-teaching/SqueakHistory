as yet unclassified
testParseSt
	|changeList|
	changeList := SourceParser parseChangeListFrom: (TestTools file: 'source.st').

	self assert: 2389 equals: (changeList changeList size).
	SourceParser showChangeList: changeList