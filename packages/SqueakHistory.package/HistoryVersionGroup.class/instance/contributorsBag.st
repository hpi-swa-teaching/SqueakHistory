as yet unclassified
contributorsBag
	^ (self changes collect: [:change | self getNameFromStamp: change stamp]) asBag