as yet unclassified
testParser2
	| p s lines filepath |
	filepath := 'git_cache' , FileDirectory slash , 'history' , FileDirectory slash , 'Squeak1.1.changes'.
	p := SourceParser newFromFile: filepath.
	s := p parse.
	lines := s findTokens: Character cr.
	
	self assert: 757414 equals: s size.
	self assert: 18215 equals: lines size