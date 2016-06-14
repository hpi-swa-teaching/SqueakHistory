as yet unclassified
getSubDirs
	| dir |
	dir := (FileDirectory default) on: dataLocation.
	^ dir directoryNames.