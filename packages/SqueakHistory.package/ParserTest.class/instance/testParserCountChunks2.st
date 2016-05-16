as yet unclassified
testParserCountChunks2
	| p chunks filepath |
	filepath := 'git_cache', FileDirectory slash, 'history', FileDirectory slash, 'Squeak1.1Short.changes'.
	p := SourceParser newFromFile: filepath.
	chunks := p countCodeChunks.

	self assert: 20 equals: chunks