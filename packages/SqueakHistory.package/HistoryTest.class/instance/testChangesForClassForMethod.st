as yet unclassified
testChangesForClassForMethod
	| history changes |
	history := TestTools history.
	changes := history changesForClass: 'AbstractSound' forMethod: 'setPitch:dur:loudness:'.
	self assert: 1 equals: changes size.
	self assert: #method equals: changes first type.
	self assert: 'AbstractSound' equals: changes first methodClass name asString.
	self assert: 'setPitch:dur:loudness:' equals: changes first methodSelector