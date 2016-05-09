as yet unclassified
parse
|ds blah|
ds := DataStream fileNamed: 'bla.txt'.
blah := ds readString.
Transcript show: blah.
ds close.