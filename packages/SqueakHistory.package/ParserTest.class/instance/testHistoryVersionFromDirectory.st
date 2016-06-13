as yet unclassified
testHistoryVersionFromDirectory
	|version|
	version := HistoryVersion newFromDirectory: (TestTools file: '1.23').
	self assert: '1.23' equals: version version.
	self assert: 5 equals: version changeLists size.
	self assert: #doIt equals: (version changeLists first changeList first type).