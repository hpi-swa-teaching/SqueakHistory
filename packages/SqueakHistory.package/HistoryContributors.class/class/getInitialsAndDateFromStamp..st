as yet unclassified
getInitialsAndDateFromStamp: stamp
	| cleanedStamp initialsAndDate |
	initialsAndDate := OrderedCollection newFrom: {'' . ''}.
	stamp isNil ifTrue: [^initialsAndDate] 
				ifFalse: [
							stamp isEmpty  ifTrue: [^initialsAndDate]
											ifFalse: [cleanedStamp := stamp replaceAll: Character cr with: Character space.
													(cleanedStamp subStrings: ' ') do: [:sub | 
														sub := sub reject: [ :c | (Character space = c) or:  (Character tab = c)].
														(sub matchesRegex: '(\d\d?/\d\d?/\d\d)') | (sub matchesRegex: '\d\d\:\d\d')
															ifTrue: [
																|dateSoFar|
																dateSoFar :=(initialsAndDate second).
																initialsAndDate at: 2 put: (dateSoFar ifEmpty: [''] ifNotEmpty: [dateSoFar, ' ']), sub]
															ifFalse: [initialsAndDate at: 1 put: sub]].
												^initialsAndDate]]