'From Squeak 1.22 of September 21, 1997 on 23 September 1997 at 1:11:08 pm'!

!InterpreterSimulator methodsFor: 'I/O primitives' stamp: 'di 9/23/97 12:54'!
primitiveKbdNext

	self pop: 1.
	Sensor keyboardPressed
		ifTrue: [self pushInteger: Sensor primKbdNext]
		ifFalse: [self push: nilObj]! !

!InterpreterSimulator methodsFor: 'I/O primitives' stamp: 'di 9/23/97 12:54'!
primitiveKbdPeek

	self pop: 1.
	Sensor keyboardPressed
		ifTrue: [self pushInteger: Sensor primKbdPeek]
		ifFalse: [self push: nilObj]! !

!InterpreterSimulator methodsFor: 'I/O primitives' stamp: 'di 9/23/97 12:54'!
primitiveMouseButtons
	| buttons |
	self pop: 1.
	buttons _ Sensor primMouseButtons.
	self pushInteger: buttons! !

!InterpreterSimulator methodsFor: 'other primitives' stamp: 'tao 9/29/97 
16:29'!
primitiveGetAttribute
	"return nil as if attribute isn't defined"
		self pop: 2.  "rcvr, attr"
		self push: (self splObj: NilObject).! !
