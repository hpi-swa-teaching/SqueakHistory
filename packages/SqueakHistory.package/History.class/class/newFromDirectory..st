as yet unclassified
newFromDirectory: directory 
	| versionsByName dir directoryNames |
	versionsByName := Dictionary new.
	dir := FileDirectory default on: directory.
	directoryNames := dir directoryNames.
	directoryNames
		do: [:each | versionsByName
				at: each
				put: (HistoryVersion
						newFromDirectory: (dir fullPathFor: each))].
	^ self newFromVersionsByName: versionsByName