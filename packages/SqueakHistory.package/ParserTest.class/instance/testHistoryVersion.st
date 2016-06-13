as yet unclassified
testHistoryVersion
	|changeList version|
	changeList := SourceParser parseChangeListFrom: (TestTools file: '1.23', FileDirectory slash, 'Squeak1.23.changes').
	version := HistoryVersion newFromVersion: '1.23' changeLists: {changeList}.
	self assert: '1.23' equals: version version.
	self assert: 1 equals: version changeLists size.
	self assert: #doIt equals: (version changeLists first changeList first type).