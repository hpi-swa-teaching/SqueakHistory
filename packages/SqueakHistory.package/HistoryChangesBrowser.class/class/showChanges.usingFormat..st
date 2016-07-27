show changes
showChanges: changes usingFormat: textFromChangeRecord
	| changeList |
	changeList := ChangeList new.
	changes do: [:changeRecord | changeList addItem: changeRecord text: (textFromChangeRecord value: changeRecord)].
	changeList listSelections.	"Necessary to make ChangeList working. Otherwise, listSelections stays nil:"
	self showChangeList: changeList