analyzing
writeContributorsToTranscript
	Transcript clear.
	self contributorsBag sortedCounts keysAndValuesDo: [:aKey :aValue |
	Transcript
		show: aKey printString;
		space;
		show: aValue printString;
		cr]