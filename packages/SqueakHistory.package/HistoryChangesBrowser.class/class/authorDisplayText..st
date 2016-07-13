as yet unclassified
authorDisplayText: changeRecord
	| author |
	author := (HistoryStampParser getNameFromStamp: changeRecord stamp).
	 ^ author ifEmpty: [''] ifNotEmpty: [' by ', author]