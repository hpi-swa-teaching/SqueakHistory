as yet unclassified
testParserCountChunks3
	| p chunks filepath |
	filepath :=TestTools file: 'SqueakV50Shorter.sources'.
	p := SourceParser newFromFile: filepath.
	chunks := p countCodeChunks.

	self assert: 27 equals: chunks