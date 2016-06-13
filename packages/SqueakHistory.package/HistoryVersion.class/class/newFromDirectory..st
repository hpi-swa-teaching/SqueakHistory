as yet unclassified
newFromDirectory: directory
|dir changeLists fileNames|
dir := (FileDirectory default) on: directory.
fileNames :=dir fileNames.
changeLists :=  fileNames collect: [:file | SourceParser parseChangeListFrom: (dir fullPathFor: file)].

^ self newFromVersion: (dir pathParts last) changeLists: changeLists.