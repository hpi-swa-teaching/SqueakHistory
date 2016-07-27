instance creation
newFromName: name changesByName: changesByName 
	^ self new versionName: name;
		 changesByName: changesByName