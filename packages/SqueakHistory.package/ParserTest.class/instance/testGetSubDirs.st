as yet unclassified
testGetSubDirs
	|parser subDirs|
	parser := SourceParser new.
	parser dataLocation: 'git_cache', FileDirectory slash, 'history'.
	subDirs := parser getSubDirs.
	self assert: #('testfolder1' 'testfolder2') equals: subDirs.