as yet unclassified
parseChangeListFrom: file 
	"inspired by ChangeLIst browseRecent: charCount on: origChangesFile"
	| stream changesFile changeList |
	stream := FileStream readOnlyFileNamed: file.
	changesFile := stream readOnlyCopy.
	changesFile setConverterForCode.
	changeList := Cursor read
				showWhile: [ChangeList new
						scanFile: changesFile
						from: 0
						to: changesFile size].
	changesFile close.
	^changeList