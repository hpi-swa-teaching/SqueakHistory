as yet unclassified
testGetDateFromStamp
	"test for the initial extraction"

	| stamp initials |
	stamp := '6/9/97 11:46 tk'.
	initials := HistoryContributors getDateFromStamp: stamp.
	self assert: '6/9/97 11:46' equals: initials.
	
	stamp := '6/9/97 tk 11:46'.
	initials := HistoryContributors getDateFromStamp: stamp.
	self assert: '6/9/97 11:46' equals: initials.
	
	stamp := 'tk 6/9/97 11:46'.
	initials := HistoryContributors getDateFromStamp: stamp.
	self assert: '6/9/97 11:46' equals: initials.
	
	stamp := 'Lorem Ipsum'.
	initials := HistoryContributors getDateFromStamp: stamp.
	self assert: '' equals: initials.

	stamp := '6/13/97 09:14 '.
	initials := HistoryContributors getDateFromStamp: stamp.
	self assert: '6/13/97 09:14' equals: initials.
	
	stamp := 'jk 7/11/2016 11:17'.
	initials := HistoryContributors getDateFromStamp: stamp.
	self assert: '7/11/2016 11:17' equals: initials.