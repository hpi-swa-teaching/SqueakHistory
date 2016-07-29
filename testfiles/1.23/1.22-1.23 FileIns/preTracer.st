'From Squeak 1.2 of June 29, 1997 on 24 September 1997 at 10:01:02 am'!
!Object methodsFor: 'comparing' stamp: 'pm 9/22/97 15:31'!
basicHash
	"Answer a SmallInteger whose value is related to the receiver's identity.
	This method must not be overridden, except by SmallInteger.
	Primitive. Fails if the receiver is a SmallInteger. Essential.
	See Object documentation whatIsAPrimitive.

	Do not override."

	<primitive: 75>
	self primitiveFailed! !

!Object methodsFor: 'comparing' stamp: 'pm 9/23/97 09:36'!
hashMappedBy: map
	"Answer what my hash would be if oops changed according to map."

	^map newHashFor: self! !


!SmallInteger methodsFor: 'comparing' stamp: 'pm 9/22/97 15:40'!
basicHash

	^self! !


!StandardFileStream methodsFor: 'added by paul' stamp: 'pm 9/22/97 15:39'!
padToNextLongPut: char 
	"Make position be on long word boundary, writing the padding 
	character, char, if necessary."
	[self position \\ 4 = 0]
		whileFalse: [self nextPut: char]! !


!Symbol methodsFor: 'comparing' stamp: 'pm 9/23/97 09:36'!
hashMappedBy: map
	"Answer what my hash would be if oops changed according to map."

	^map newHashFor: self! !


