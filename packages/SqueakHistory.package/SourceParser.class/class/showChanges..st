as yet unclassified
showChanges: changes
	|changeList|
	changeList := ChangeList new.
	changes do: [ :changeRecord | changeList addItem: changeRecord text: changeRecord methodClass name, ' ', changeRecord methodSelector, ' by ', (HistoryCounter getNameFromStamp: changeRecord stamp)].
	changeList listSelections.
	self showChangeList: changeList