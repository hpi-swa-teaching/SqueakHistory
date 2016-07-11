as yet unclassified
changesForClass: aClass selector: aSelector
	"Inspired by VersionsBrowser browseVersionsForClass:  selector:"
	^ self
		changesFor: (aClass compiledMethodAt: aSelector)
		class: aClass
		meta: aClass isMeta
		category: (aClass organization categoryOfElement: aSelector)
		selector: aSelector