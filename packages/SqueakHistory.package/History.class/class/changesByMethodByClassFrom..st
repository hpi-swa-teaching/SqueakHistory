instance creation
changesByMethodByClassFrom: changes
	| groupedByClass |
	groupedByClass := changes groupBy: [:changeRecord | changeRecord methodClass name asString].
	^ Dictionary
		newFrom: (groupedByClass associations
			collect: [:each | each key -> (each value
				groupBy: [:changeRecord | changeRecord methodSelector])])