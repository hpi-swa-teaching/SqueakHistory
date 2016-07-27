parsing
getInitialsAndDateStringFromStamp: stamp 
	| initialsAndDate substrings |
	(stamp isNil or: [stamp isEmpty]) ifTrue: [^ {''. ''}].
	
	initialsAndDate := {'' . ''}.
	substrings := (stamp subStrings: (' ', Character cr, Character tab)).
	substrings do: [:sub | initialsAndDate := self parse: initialsAndDate from: sub].
	^ initialsAndDate