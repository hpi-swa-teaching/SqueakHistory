as yet unclassified
testChangesWithoutRecentForClassForMethod
	| history changes |
	history := HistoryTestTools history.
	changes := history changesWithoutRecentForClass: 'AbstractSound' forMethod: 'setPitch:dur:loudness:'.
	self assert: 1 equals: changes size.
	self assert: #method equals: changes first type.
	self assert: 'AbstractSound' equals: changes first methodClass name asString.
	self assert: 'setPitch:dur:loudness:' equals: changes first methodSelector.
	
	HistoryChangesBrowser showChanges: changes.
	HistoryChangesBrowser showChangesWithDefaultFormatting: changes