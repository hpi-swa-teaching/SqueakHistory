as yet unclassified
contributorsBagFrom: changes
	^ (changes collect: [:changeRecord | self getNameFromStamp: changeRecord stamp]) asBag