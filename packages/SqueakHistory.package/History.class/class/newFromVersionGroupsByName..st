instance creation
newFromVersionGroupsByName: versionGroupsByName 
	| changes changesByMethodByClass |
	changes := self gatherChanges: versionGroupsByName.
	changesByMethodByClass := self changesByMethodByClassFrom: changes.
	^ self new versionGroupsByName: versionGroupsByName;
		 changesByMethodByClass: changesByMethodByClass; changes: changes