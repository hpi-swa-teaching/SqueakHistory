as yet unclassified
displayTextByType
	^ IdentityDictionary newFrom: {
		#doIt -> [:changeRecord | 'Comment'] . 
		#method -> [:changeRecord | (self classDisplayText: changeRecord), changeRecord methodSelector] .
		#classComment -> [:changeRecord | self classDisplayText: changeRecord]}