as yet unclassified
testParseChanges
	| changeList|
	changeList := (SourceParser parseChangeListFrom: (TestTools file: 'test.changes')) changeList.
	SourceParser parseChangeListFrom: (TestTools file: 'Squeak1.1.changes').

	self assert: 6 equals: changeList size.
	self assert:  '''From Squeak5.1 of 26 April 2016 [latest update: #15852] on 9 May 2016 at 11:41:01 am''' equals: changeList first string.
	self assert: 'TestCase subclass: #MethodTest
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''SqueakHistory-Tests''' equals: changeList second string.
	self assert:  'successMethod
	"here success stuff"' equals: changeList third string.
	
	self assert: 'rb 6/23/2016 24:15' equals: changeList fifth stamp.
	self assert: (TimeStamp fromString: '23 June 2016 12:15 am') equals: changeList fifth timeStamp