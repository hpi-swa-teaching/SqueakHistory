accessing
changesWithoutRecentForClass: className forMethod: methodSelector
	^ (self changesByMethodByClass at: className) at: methodSelector