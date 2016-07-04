as yet unclassified
parseChangesFrom: file
	"inspired by ChangeList browseRecent: charCount on: origChangesFile"
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
	^changeList changeList