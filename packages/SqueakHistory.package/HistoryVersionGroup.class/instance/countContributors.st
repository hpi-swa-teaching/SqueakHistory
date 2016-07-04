as yet unclassified
countContributors 
	^ HistoryContributors countContributorsFrom: (self versionsByName
				gather: [:historyVersion | historyVersion changesByName
						gather: [:changes | changes]])