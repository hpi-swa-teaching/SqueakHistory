as yet unclassified
getTopContributorsFromArchive
	"comment stating purpose of message"

	| con |
	con := self countContributorsFrom: (History instance changes).
	
	^(con associations asSortedCollection: [:x :y | x value > y value])