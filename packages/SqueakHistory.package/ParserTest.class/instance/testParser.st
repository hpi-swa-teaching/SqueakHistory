as yet unclassified
testParser
|p filepath|
filepath := 'git_cache', FileDirectory slash, 'history', FileDirectory slash, 'testfile.txt'.
p:=SourceParser newFromFile: filepath.
	self assert: 'teststring' withCRs equals: p parse.