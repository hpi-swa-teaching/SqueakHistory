testing
testGetNameFromStamp
	| stamp name |
	stamp := 'mt 7/2/2010 08:04:20.957'.
	name := HistoryStampParser getNameFromStamp: stamp.
	self assert: 'mt' equals: name.
	
	stamp := 'mt 7/2/2010 08:04:20.95'.
	name := HistoryStampParser getNameFromStamp: stamp.
	self assert: 'mt' equals: name.
	
	stamp := 'mt 7/2/2010 08:04:20.9'.
	name := HistoryStampParser getNameFromStamp: stamp.
	self assert: 'mt' equals: name