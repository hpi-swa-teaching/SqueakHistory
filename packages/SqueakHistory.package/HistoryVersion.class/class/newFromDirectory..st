as yet unclassified
newFromDirectory: directory
	| dir changesByName |
	dir := (FileDirectory default) on: directory.
	changesByName := Dictionary newFrom: (dir fileNames collect: [:file | file -> (HistoryChangesParser parseChangesFrom: (dir fullPathFor: file))]).

	^ self newFromName: (dir pathParts last) changesByName: changesByName.