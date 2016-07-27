*SqueakHistory
browseVersions
	| class |
	self classCommentIndicated
		ifTrue: [ClassCommentVersionsBrowser browseCommentOf: self selectedClass.
			^ nil].

	class := self selectedClassOrMetaClass.
	^ self selectedMessageName
		ifNil: [self inform: 'Sorry, only actual methods have retrievable versions.'.
			nil]
		ifNotNil: [:methodName | HistoryChangesBrowser
				showChanges: (History isInstanceLoaded
						ifTrue: [HistoryRecentChangesParser changesForClass: class selector: methodName]
						ifFalse: [[History instance changesForClass: class forMethod: methodName]
								on: KeyNotFound
								do: [History instance changesForClass: class forMethod: methodName]])]