accessing
changesForClass: class forMethod: methodSelector
	^ (self changesWithoutRecentForClass: class name asString forMethod: methodSelector asString), (HistoryRecentChangesParser changesForClass: class selector: methodSelector)