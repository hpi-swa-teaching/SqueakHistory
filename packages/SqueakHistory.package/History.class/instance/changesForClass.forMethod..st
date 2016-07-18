analyzing
changesForClass: class forMethod: methodSelector
	^ ((self changesWithoutRecentForClass: class asString 
	forMethod: methodSelector asString),
		(HistoryRecentChangesParser changesForClass: class selector: methodSelector)) 			sortBy: 	[:a :b | (HistoryStampParser getTimeStampFromStamp: a stamp) > 
				(HistoryStampParser getTimeStampFromStamp: b stamp)]