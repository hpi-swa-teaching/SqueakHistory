testing
testGetDateFromStamp
	| timeStamp |
	timeStamp := HistoryStampParser getTimeStampFromDateString: '6/9/97 11:46'.
	self assert: '9 June 1997 11:46 am' equals: timeStamp asString.
	timeStamp := HistoryStampParser getTimeStampFromDateString: '7/11/2016 11:17'.
	self assert: '11 July 2016 11:17 am' equals: timeStamp asString.
	
	timeStamp := HistoryStampParser getTimeStampFromDateString: ''.
	self assert: '1 January 1901 12:00 am' equals: timeStamp asString.
	
	timeStamp := HistoryStampParser getTimeStampFromDateString: '11:17'.
	self assert: '1 January 1901 12:00 am' equals: timeStamp asString.
	
	timeStamp := HistoryStampParser getTimeStampFromDateString: '7/11/2016'.
	self assert: '1 January 1901 12:00 am' equals: timeStamp asString