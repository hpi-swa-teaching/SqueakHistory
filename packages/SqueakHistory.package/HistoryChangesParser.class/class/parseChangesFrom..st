parsing
parseChangesFrom: file
	"inspired by ChangeList browseRecent: charCount on: origChangesFile"
	| changesFile changeList |
	changesFile := (FileStream readOnlyFileNamed: file) readOnlyCopy.
	changesFile setConverterForCode.
	changeList := Cursor read showWhile: [
			ChangeList new
						scanFile: changesFile
						from: 0
						to: changesFile size].
	changesFile close.
	^ changeList changeList