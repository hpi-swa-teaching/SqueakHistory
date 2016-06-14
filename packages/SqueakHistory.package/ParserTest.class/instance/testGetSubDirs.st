as yet unclassified
testGetSubDirs
	| parser subDirs |
	parser := SourceParser new.
	parser dataLocation: 'git_cache', FileDirectory slash, 'history', FileDirectory slash, '1.23'.
	subDirs := parser getSubDirs.
	self assert: #('1.22-1.23 FileIns' 'Webtalk') equals: subDirs.