as yet unclassified
testParserCountChunks2
	| p chunks filepath |
	filepath := TestTools file: 'Squeak1.1Short.changes'.
	p := SourceParser newFromFile: filepath.
	chunks := p countCodeChunks.

	self assert: 20 equals: chunks