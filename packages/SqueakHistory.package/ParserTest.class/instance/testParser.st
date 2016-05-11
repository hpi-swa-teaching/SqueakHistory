as yet unclassified
testParser
|p|
p:=SourceParser newFromFile: 'history\bla.txt'.
	self assert: 'Üäääh?\\Ö' withCRs equals: p parse.