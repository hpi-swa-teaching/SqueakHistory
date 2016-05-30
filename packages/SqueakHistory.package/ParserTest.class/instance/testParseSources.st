as yet unclassified
testParseSources
	|changeList|
	changeList := SourceParser parseChangeListFrom: (TestTools file: 'SqueakV50Short.sources').
	.
	self assert: 18 equals: (changeList changeList size).
	