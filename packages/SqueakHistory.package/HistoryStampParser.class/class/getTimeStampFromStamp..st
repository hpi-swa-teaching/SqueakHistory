parsing
getTimeStampFromStamp: string
	^ self getTimeStampFromDateString: (self getDateStringFromStamp: string)