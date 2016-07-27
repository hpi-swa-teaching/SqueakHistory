analyzing
changes
	^ self versionsByName
		gather: [:historyVersion | historyVersion changesByName
				gather: [:changes | changes]]