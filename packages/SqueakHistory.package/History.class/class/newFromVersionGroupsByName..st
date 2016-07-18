instance creation
newFromVersionGroupsByName: versionGroupsByName 
	| changes changesByMethodByClass |
	changes := self gatherChanges: versionGroupsByName.
	changesByMethodByClass := self changesByMethodByClass: changes.
	^ self new versionGroupsByName: versionGroupsByName;
		 changesByMethodByClass: changesByMethodByClass; changes: changes