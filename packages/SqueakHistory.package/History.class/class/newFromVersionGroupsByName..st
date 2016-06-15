as yet unclassified
newFromVersionGroupsByName: versionGroupsByName 
	| changesByMethodByClass changeRecords groupedByClass |
	changeRecords := versionGroupsByName values
				gather: [:versionGroup | versionGroup versionsByName values
						gather: [:version | version changesByName values
								gather: [:changeList | changeList changeList]]].
	groupedByClass := changeRecords
				groupBy: [:changeRecord | changeRecord methodClass name asString].
	changesByMethodByClass := Dictionary
				newFrom: (groupedByClass associations
						collect: [:each | each key -> (each value
								groupBy: [:changeRecord | changeRecord methodSelector])]).
	^ self new versionGroupsByName: versionGroupsByName;
		 changesByMethodByClass: changesByMethodByClass