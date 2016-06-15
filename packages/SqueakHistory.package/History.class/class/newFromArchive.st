as yet unclassified
newFromArchive
	"should not be tested, because it uses the archive"
	| directory |
	directory := 'git_cache' , FileDirectory slash , 'SqueakHistoryArchive' , FileDirectory slash , 'data'.
	^ self newFromDirectory: directory