as yet unclassified
newFromDirectory: directory
|dir fileNames changesByName |
changesByName := Dictionary new.
dir := (FileDirectory default) on: directory.
fileNames :=dir fileNames.
fileNames do: [:file |	changesByName at: file put: (SourceParser parseChangeListFrom: (dir fullPathFor: file))].

^ self newFromName: (dir pathParts last) changesByName: changesByName.