as yet unclassified
newFromDirectory: directory 
	| versionsByName dir |
	dir := FileDirectory default on: directory.
	versionsByName := Dictionary newFrom: (dir directoryNames collect: [:file | file -> (HistoryVersion newFromDirectory: (dir fullPathFor: file))])..
	^ self newFromVersionsByName: versionsByName