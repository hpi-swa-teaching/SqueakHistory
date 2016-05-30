as yet unclassified
testChangeList
	| changeList changesFile file stream |
	file := 'git_cache' , FileDirectory slash , 'history' , FileDirectory slash , 'Squeak1.1.changes'.
	stream := FileStream readOnlyFileNamed: file.
	"inspired by ChangeLIst browseRecent: charCount on: origChangesFile"
	changesFile := stream readOnlyCopy.
	changesFile setConverterForCode.
	changeList := Cursor read
				showWhile: [ChangeList new
						scanFile: changesFile
						from: 0
						to: changesFile size].
	changesFile close.