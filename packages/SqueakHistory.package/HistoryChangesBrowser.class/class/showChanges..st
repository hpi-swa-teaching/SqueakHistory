as yet unclassified
showChanges: changes 
	self showChanges: changes usingFormat: 
		[:changeRecord | changeRecord methodClass name , ' ' , changeRecord methodSelector , ' by '
				, (HistoryContributors getNameFromStamp: changeRecord stamp)]