instance creation
gatherChanges: versionGroupsByName
 	^ versionGroupsByName values
		gather: [:versionGroup | versionGroup versionsByName values
			gather: [:version | version changesByName values
				gather: [:changeRecord | changeRecord]]]