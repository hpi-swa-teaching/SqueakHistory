show changes
showChanges: changes usingFormat: textFromChangeRecord
	|changeList|
	changeList := ChangeList new.
	changes do: [ :changeRecord | changeList addItem: changeRecord text: (textFromChangeRecord value: changeRecord)].
	"Needed to make ChangeList working. Otherwise, listSelections stays nil:"
	changeList listSelections.
	self showChangeList: changeList