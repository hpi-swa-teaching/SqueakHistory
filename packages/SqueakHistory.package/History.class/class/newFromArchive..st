as yet unclassified
newFromArchive: versionGroup
	"should not be tested, because it uses the archive"
	
	| directory |
	directory := 'git_cache' , FileDirectory slash , 'SqueakHistoryArchive' , FileDirectory slash , 'data', FileDirectory slash, versionGroup.
	^ self newFromDirectory: directory