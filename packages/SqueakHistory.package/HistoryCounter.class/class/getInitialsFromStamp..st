as yet unclassified
getInitialsFromStamp: stamp
	"comment stating purpose of message"
	| cleanedStamp |
	stamp isNil ifTrue: [^''] 
				ifFalse: [
							stamp isEmpty  ifTrue: [^'']
											ifFalse: [cleanedStamp := stamp replaceAll: Character cr with: Character space.
													(cleanedStamp subStrings: ' ') do: [:sub | 
														sub := sub replaceAll: Character tab with: Character null.
														(sub matchesRegex: '(\d\d?/\d\d?/\d\d)') not & (sub matchesRegex: '\d\d\:\d\d') not ifTrue: [^sub]].
												^'']].
	
	