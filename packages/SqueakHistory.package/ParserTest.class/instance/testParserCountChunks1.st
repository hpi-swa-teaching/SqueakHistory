as yet unclassified
testParserCountChunks1
	| p chunks filepath |
	filepath := TestTools file: 'SqueakV50Short.sources'.
	p := SourceParser newFromFile: filepath.
	chunks := p countCodeChunks.

	self assert: 57 equals: chunks