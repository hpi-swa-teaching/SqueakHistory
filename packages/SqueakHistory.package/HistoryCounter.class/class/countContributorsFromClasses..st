as yet unclassified
countContributorsFromClasses: aClassesDictionary
	"counts Contributors"
	
	| allChanges |
	
	allChanges := OrderedCollection new.
	aClassesDictionary do: [:classes | 
		classes do: [:methods | allChanges addAll: methods]].
	
	^self countContributorsFromDictionary: allChanges