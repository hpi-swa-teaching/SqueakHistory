accessing
contributorsBag
	^ (self changes collect: [:change | HistoryStampParser getNameFromStamp: change stamp]) asBag