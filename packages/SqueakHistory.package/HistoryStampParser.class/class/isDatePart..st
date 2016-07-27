parsing
isDatePart: string
	^	(string matchesRegex: '(\d\d?/\d\d?/\d?\d?\d\d)') or:
		[string matchesRegex: '\d\d\:\d\d']  or:
		[string matchesRegex: '\d\d\:\d\d\:\d\d(.\d\d?\d?)?']