as yet unclassified
testReturnClass
	"would open windows while testing"
	"| returnValue |
	History newFromDirectory: (TestTools file: '1.16 - 1.3').	
	returnValue := ChangesBrowser browseChangesFromVersion: '1.16'.
	self assert: returnValue class equals: Browser"