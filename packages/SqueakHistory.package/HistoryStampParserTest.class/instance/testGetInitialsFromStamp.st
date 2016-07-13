as yet unclassified
testGetInitialsFromStamp
	"test for the initial extraction"

	| stamp initials |
	stamp := '6/9/97 11:46 tk'.
	initials := HistoryStampParser getInitialsFromStamp: stamp.
	self assert: 'tk' equals: initials.
	
	stamp := '6/9/97 tk 11:46'.
	initials := HistoryStampParser getInitialsFromStamp: stamp.
	self assert: 'tk' equals: initials.
	
	stamp := 'tk 6/9/97 11:46'.
	initials := HistoryStampParser getInitialsFromStamp: stamp.
	self assert: 'tk' equals: initials.
	
	stamp := 'Lorem Ipsum'.
	initials := HistoryStampParser getInitialsFromStamp: stamp.
	self assert: 'Ipsum' equals: initials.

	stamp := '6/13/97 09:14 '.
	initials := HistoryStampParser getInitialsFromStamp: stamp.
	self assert: '' equals: initials.
	
	stamp := 'jk 7/11/2016 11:17'.
	initials := HistoryStampParser getInitialsFromStamp: stamp.
	self assert: 'jk' equals: initials.
	
	stamp := 'mt 7/2/2010 08:04:20.957'.
	initials := HistoryStampParser getNameFromStamp: stamp.
	self assert: 'mt' equals: initials.
	
	stamp := 'mt 7/2/2010 08:04:20.95'.
	initials := HistoryStampParser getNameFromStamp: stamp.
	self assert: 'mt' equals: initials.
	
	stamp := 'mt 7/2/2010 08:04:20.9'.
	initials := HistoryStampParser getNameFromStamp: stamp.
	self assert: 'mt' equals: initials.