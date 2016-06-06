as yet unclassified
testParseChanges
	| changeList|
	changeList := (SourceParser parseChangeListFrom: (TestTools file: 'test.changes')) changeList.
	SourceParser parseChangeListFrom: (TestTools file: 'Squeak1.1.changes').
	self assert: 7 equals: changeList size.
	self assert:  '''From Squeak5.1 of 26 April 2016 [latest update: #15852] on 9 May 2016 at 11:41:01 am''' equals: changeList first string.
	self assert: #doIt equals: changeList first type.
	self assert: 'TestCase subclass: #MethodTest
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''SqueakHistory-Tests''' equals: changeList second string.
	self assert: #doIt equals: changeList second type.
	self assert:  'successMethod
	"here success stuff"' equals: changeList third string.
	self assert: #method equals: changeList third type.
	
	self assert: 'rb 2/31/2016 10:15' equals: changeList fourth stamp.
	self assert: 1901 equals: changeList fourth timeStamp year.  "Does not work: 
	 (TimeStamp fromString: '1 January 1901 12:00 am') 
	self should: [changeList fourth timeStamp] raise: Error whoseDescriptionIncludes: 'day is after month ends.' description: 'bla'."
	self assert:  'failMethod1
	"commentary with 3 !"' equals: changeList fourth string.
	
	self assert: 'rb 6/23/2016 50:15' equals: changeList fifth stamp.
	self assert: (TimeStamp fromString: '23 June 2016 2:15 am') equals: changeList fifth timeStamp.
	
	self assert: #classComment equals: changeList sixth type.
	self assert: 'blabla!' equals: changeList sixth string.