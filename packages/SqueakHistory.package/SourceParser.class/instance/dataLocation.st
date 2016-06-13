accessing
dataLocation
	^ dataLocation ifNil: (dataLocation := 'git_cache', FileDirectory slash, 'SqueakHistoryArchive', FileDirectory slash, 'data').