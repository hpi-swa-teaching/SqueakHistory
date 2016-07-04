as yet unclassified
countContributors 
	^ HistoryContributors contributorsBagFrom: (self versionsByName
				gather: [:historyVersion | historyVersion changesByName
						gather: [:changes | changes]])