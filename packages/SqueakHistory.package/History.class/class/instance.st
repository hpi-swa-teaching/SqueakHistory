access
instance
	instance ifNil: [^ self error: 'No history loaded.'].
	^ instance