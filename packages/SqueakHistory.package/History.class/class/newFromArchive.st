instance creation
newFromArchive
	| directory |
	directory := 'git_cache' , FileDirectory slash , 'SqueakHistoryArchive' , FileDirectory slash , 'data'.
	^ self newFromDirectory: directory