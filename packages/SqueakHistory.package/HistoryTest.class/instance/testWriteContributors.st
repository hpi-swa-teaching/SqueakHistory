as yet unclassified
testWriteContributors
	| sortedCounts |
	
	sortedCounts := HistoryTestTools history contributorsBag sortedCounts.
	self assert: 6145 equals: sortedCounts first key.
	self assert: '' equals: sortedCounts first value.
	self assert: 1174 equals: sortedCounts second key.
	self assert: 'John Maloney' equals: sortedCounts second value