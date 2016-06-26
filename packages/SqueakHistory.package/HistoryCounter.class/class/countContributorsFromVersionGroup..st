as yet unclassified
countContributorsFromVersionGroup: aHistoryVersionGroup
	"counts Contributors"
	
	| allChanges|
	
	allChanges := OrderedCollection new.
	aHistoryVersionGroup versionsByName do: [:aHistoryVersion | 
		aHistoryVersion changesByName do: [:changeList |
			 allChanges addAll: changeList changeList ]].

	^self countContributorsFromDictionary: allChanges