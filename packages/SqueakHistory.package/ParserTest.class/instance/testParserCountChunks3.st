as yet unclassified
testParserCountChunks3
	| p chunks filepath |
	filepath := 'git_cache', FileDirectory slash, 'history', FileDirectory slash, 'SqueakV50Shorter.sources'.
	p := SourceParser newFromFile: filepath.
	chunks := p countCodeChunks.

	self assert: 27 equals: chunks