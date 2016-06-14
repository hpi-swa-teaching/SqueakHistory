as yet unclassified
browseChangesFromVersion: versionNumber
	| history historyVersion |
	history := History instance.
	history ifNil: [^ self error: 'No history loaded.'].
	[historyVersion := (history versionsByName at: versionNumber)]
		ifError: [^ self error: 'This version is not loaded.'].
	historyVersion changesByName  do: [ :changeList |
		^ SourceParser showChangeList: changeList ] 