as yet unclassified
testParserCountChunks1
	| p chunks filepath |
	filepath := 'git_cache', FileDirectory slash, 'history', FileDirectory slash, 'SqueakV50Short.sources'.
	p := SourceParser newFromFile: filepath.
	chunks := p countCodeChunks.

	self assert: 57 equals: chunks