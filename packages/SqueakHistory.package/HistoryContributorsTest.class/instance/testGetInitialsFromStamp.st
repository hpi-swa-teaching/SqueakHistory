as yet unclassified
testGetInitialsFromStamp
	"test for the initial extraction"

	| stamp initials |
	stamp := '6/9/97 11:46 tk'.
	initials := HistoryContributors getInitialsFromStamp: stamp.
	self assert: 'tk' equals: initials.
	
	stamp := '6/9/97 tk 11:46'.
	initials := HistoryContributors getInitialsFromStamp: stamp.
	self assert: 'tk' equals: initials.
	
	stamp := 'tk 6/9/97 11:46'.
	initials := HistoryContributors getInitialsFromStamp: stamp.
	self assert: 'tk' equals: initials.
	
	stamp := 'Lorem Ipsum'.
	initials := HistoryContributors getInitialsFromStamp: stamp.
	self assert: 'Ipsum' equals: initials.

stamp := '6/13/97 09:14 '.
	initials := HistoryContributors getInitialsFromStamp: stamp.
	self assert: '' equals: initials.
	