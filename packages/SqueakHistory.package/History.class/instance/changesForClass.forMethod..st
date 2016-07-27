analyzing
changesForClass: class forMethod: methodSelector
	| oldChanges recentChanges |
	oldChanges := self changesWithoutRecentForClass: class asString 
						forMethod: methodSelector asString.
	recentChanges := (HistoryRecentChangesParser changesForClass: class selector: methodSelector)
							sortBy: 	[:a :b |
								(HistoryStampParser getTimeStampFromStamp: a stamp) > 
								(HistoryStampParser getTimeStampFromStamp: b stamp)].
	^ (oldChanges, recentChanges)