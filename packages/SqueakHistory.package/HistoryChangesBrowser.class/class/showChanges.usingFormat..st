as yet unclassified
showChanges: changes usingFormat: textFromChangeRecord
	|changeList|
	changeList := ChangeList new.
	changes do: [ :changeRecord | changeList addItem: changeRecord text: (textFromChangeRecord value: changeRecord)].
	changeList listSelections.
	self showChangeList: changeList