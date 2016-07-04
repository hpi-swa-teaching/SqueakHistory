as yet unclassified
testParseChanges
	| changes|
	changes := (HistoryChangesParser parseChangesFrom: (HistoryTestTools file: 'test.changes')).
	HistoryChangesParser parseChangesFrom: (HistoryTestTools file: 'Squeak1.1.changes').
	self assert: 7 equals: changes size.
	self assert:  '''From Squeak5.1 of 26 April 2016 [latest update: #15852] on 9 May 2016 at 11:41:01 am''' equals: changes first string.
	self assert: #doIt equals: changes first type.
	self assert: 'TestCase subclass: #MethodTest
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''SqueakHistory-Tests''' equals: changes second string.
	self assert: #doIt equals: changes second type.
	self assert:  'successMethod
	"here success stuff"' equals: changes third string.
	self assert: #method equals: changes third type.
	
	self assert: 'rb 2/31/2016 10:15' equals: changes fourth stamp.
	self assert: 1901 equals: changes fourth timeStamp year.  "Does not work: 
	 (TimeStamp fromString: '1 January 1901 12:00 am') 
	self should: [changeList fourth timeStamp] raise: Error whoseDescriptionIncludes: 'day is after month ends.' description: 'bla'."
	self assert:  'failMethod1
	"commentary with 3 !"' equals: changes fourth string.
	
	self assert: 'rb 6/23/2016 50:15' equals: changes fifth stamp.
	self assert: (TimeStamp fromString: '23 June 2016 2:15 am') equals: changes fifth timeStamp.
	
	self assert: #classComment equals: changes sixth type.
	self assert: 'blabla!' equals: changes sixth string.