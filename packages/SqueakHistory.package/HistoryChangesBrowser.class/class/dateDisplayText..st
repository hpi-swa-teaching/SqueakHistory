as yet unclassified
dateDisplayText: changeRecord
	| date |
	date := HistoryContributors getDateFromStamp: changeRecord stamp.
	 ^ date ifNil: [''] ifNotNil: [date ifEmpty: [''] ifNotEmpty: [' from ', date]]