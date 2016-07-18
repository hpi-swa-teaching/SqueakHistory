parsing
isDatePart: string
	^(string matchesRegex: '(\d\d?/\d\d?/\d?\d?\d\d)') | 
	(string matchesRegex: '\d\d\:\d\d')  | 
	(string matchesRegex: '\d\d\:\d\d\:\d\d(.\d\d?\d?)?')