as yet unclassified
testParser2
|p s lines|
p:=SourceParser newFromFile: 'history', FileDirectory slash, 'Squeak1.1.changes'.
s:=p parse.
lines := s findTokens: Character cr.

"lines withIndexCollect: [:line :index |  ]".

self assert: 757414 equals: s size.
self assert: 18215 equals: lines size