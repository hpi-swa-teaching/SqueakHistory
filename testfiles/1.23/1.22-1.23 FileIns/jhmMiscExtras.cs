'From Squeak 1.22 of September 21, 1997 on 4 October 1997 at 8:49:18 am'!
'From Squeak 1.22 of September 21, 1997 on 3 October 1997 at 3:07:43 pm'!
SketchMorph subclass: #ColorPickerMorph
	instanceVariableNames: 'selectedColor sourceHand deleteOnMouseUp updateContinuously selector target '
	classVariableNames: 'ColorChart FeedbackBox TransparentBox '
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!HTTPSocket class methodsFor: 'proxy settings'!
useProxyServerNamed: proxyServerName port: portNum
	"Direct all HTTP requests to the HTTP proxy server with the given name and port number."
	"HTTPSocket useProxyServerNamed: 'web-proxy.disney.com' port: 8080"
	proxyServerName ifNil: [  "clear proxy settings"
		HTTPProxy _ nil.
		HTTPPort _ 80.
		^ self].

	proxyServerName class == String
		ifFalse: [self error: 'Server name must be a String or nil'].
	HTTPProxy _ proxyServerName.

	HTTPPort _ portNum.
	HTTPPort class == String ifTrue: [HTTPPort _ portNum asNumber].
	HTTPPort ifNil: [HTTPPort _ 80].
! !
HandMorph removeSelector: #startRunningAll!
HandMorph removeSelector: #saveWorldTest!
HandMorph removeSelector: #stopRunningAll!
HandMorph removeSelector: #saveEToyInFile!
HandMorph removeSelector: #stepAll!
