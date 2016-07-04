as yet unclassified
countContributorsFrom: changes
	^ changes collect: [:changeRecord | self getNameFromStamp: changeRecord stamp] asBag