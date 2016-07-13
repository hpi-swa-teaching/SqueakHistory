as yet unclassified
dateDisplayText: changeRecord
	| date |
	date := HistoryStampParser getDateStringFromStamp: changeRecord stamp.
	 ^ date ifNil: [''] ifNotNil: [date ifEmpty: [''] ifNotEmpty: [' from ', date]]