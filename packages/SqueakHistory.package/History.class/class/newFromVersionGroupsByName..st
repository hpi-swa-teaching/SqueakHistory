as yet unclassified
newFromVersionGroupsByName: versionGroupsByName 
	| changes changesByMethodByClass groupedByClass |
	changes := versionGroupsByName values
				gather: [:versionGroup | versionGroup versionsByName values
						gather: [:version | version changesByName values
								gather: [:changeRecord | changeRecord]]].
	groupedByClass := changes
				groupBy: [:changeRecord | changeRecord methodClass name asString].
	changesByMethodByClass := Dictionary
				newFrom: (groupedByClass associations
						collect: [:each | each key -> (each value
								groupBy: [:changeRecord | changeRecord methodSelector])]).
	^ self new versionGroupsByName: versionGroupsByName;
		 changesByMethodByClass: changesByMethodByClass; changes: changes