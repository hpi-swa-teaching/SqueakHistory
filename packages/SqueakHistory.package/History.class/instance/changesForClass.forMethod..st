accessing
changesForClass: class forMethod: methodSelector
	^ (self changesWithoutRecentForClass: class asString forMethod: methodSelector asString), (HistoryRecentChangesParser changesForClass: class selector: methodSelector)