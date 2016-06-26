as yet unclassified
getTopContributorsFromArchive
	"comment stating purpose of message"

	| con |
	con := self countContributorsFromClasses: (History instance changesByMethodByClass).
	
	^(con associations asSortedCollection: [:x :y | x value > y value])