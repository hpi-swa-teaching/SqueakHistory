as yet unclassified
changes
	^ self versionsByName gather: [:historyVersion | historyVersion changesByName gather: [:changes | changes]]