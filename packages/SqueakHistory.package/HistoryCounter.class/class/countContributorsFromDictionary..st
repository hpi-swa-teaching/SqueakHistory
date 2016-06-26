as yet unclassified
countContributorsFromDictionary: aChangesDictionary

	
	| contributors author|
	contributors := Dictionary new.	
	aChangesDictionary do: [:changes |
		author := self getNameFromStamp: changes stamp.
				(contributors includesKey: author)
					ifTrue: [contributors at: author put: ((contributors at: author) + 1)]																														 					ifFalse: [contributors add: author -> 1]].
	^contributors