as yet unclassified
toTranscript: aContributorDictionary
	"prints Contributors to transcript"

	Transcript clear.
	aContributorDictionary keysAndValuesDo: [:aKey :aValue |
	Transcript
		show: aKey printString;
		space;
		show: aValue printString;
		cr]
	