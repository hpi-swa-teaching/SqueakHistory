*SqueakHistory
browseVersions
|methodName class|
self classCommentIndicated ifTrue:
	[ClassCommentVersionsBrowser browseCommentOf: self selectedClass.
	 ^nil].

class := self selectedClassOrMetaClass.
^(methodName := self selectedMessageName)
	ifNil: [self inform: 'Sorry, only actual methods have retrievable versions.'. nil]
	ifNotNil: [	
		HistoryChangesBrowser showChanges: 
			(History isInstanceLoaded 
				ifFalse: [HistoryRecentChangesParser changesForClass: class selector: methodName]
				ifTrue: [[History instance changesForClass: class forMethod: methodName] on: KeyNotFound do: 
					[History instance changesForClass: class forMethod: methodName]])]