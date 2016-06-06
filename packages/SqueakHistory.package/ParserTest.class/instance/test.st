as yet unclassified
test
	|types changeList|
	changeList:=SourceParser parseChangeListFrom: (TestTools file: 'source.st').
	SourceParser parseChangeListFrom: (TestTools file: 'Squeak1.1.changes').

types := changeList changeList groupBy: [:it | it type].
	