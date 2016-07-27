analyzing
contributorsBag
	^ (self changes
		collect: [:change | self getNameFromStamp: change stamp]) asBag