instance creation
newFromDirectory: directory 
	| versionGroupsByName dir |
	dir := FileDirectory default on: directory.
	versionGroupsByName := Dictionary newFrom: (dir directoryNames collect: [:file | 
		file -> (HistoryVersionGroup newFromDirectory: (dir fullPathFor: file))])..
	^ self newFromVersionGroupsByName: versionGroupsByName