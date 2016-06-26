as yet unclassified
countContributorsFromVersion: aChangesDictionary
	"counts Contributors"
	
	| contributors author|
	
	Transcript clear.
	contributors := Dictionary new.
	aChangesDictionary do: [:classes | 
		classes do: [:methods |
			methods do: [:changes |
				author := self getInitialsFromStamp: changes stamp.
				(contributors includesKey: author)
					ifTrue: [contributors at: author put: ((contributors at: author) + 1)]																														 					ifFalse: [contributors add: author -> 1]]]].
	^contributors