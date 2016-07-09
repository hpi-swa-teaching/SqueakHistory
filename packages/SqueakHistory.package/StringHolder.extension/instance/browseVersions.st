*SqueakHistory
browseVersions
|methodName this|
self classCommentIndicated ifTrue:
	[ClassCommentVersionsBrowser browseCommentOf: self selectedClass.
	 ^nil].

this := self.
^(methodName := self selectedMessageName)
	ifNil: [self inform: 'Sorry, only actual methods have retrievable versions.'. nil]
	ifNotNil: [HistoryChangesBrowser showChanges: (History instance changesForClass: this selectedClassName forMethod: methodName)]