as yet unclassified
authorDisplayText: changeRecord
	| author |
	author := (HistoryContributors getNameFromStamp: changeRecord stamp).
	 ^ author ifEmpty: [''] ifNotEmpty: [' by ', author]