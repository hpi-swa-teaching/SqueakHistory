'From Squeak 1.22 of September 21, 1997 on 28 September 1997 at 3:35:06 pm'!

!Object methodsFor: 'comparing' stamp: 'di 9/27/97 20:27'!
identityHash
	"Answer a SmallInteger whose value is related to the receiver's identity.
	This method must not be overridden, except by SmallInteger.
	Primitive. Fails if the receiver is a SmallInteger. Essential.
	See Object documentation whatIsAPrimitive.

	Do not override."

	<primitive: 75>
	self primitiveFailed! !

!Object methodsFor: 'comparing' stamp: 'di 9/27/97 20:23'!
identityHashMappedBy: map
	"Answer what my hash would be if oops changed according to map."

	^map newHashFor: self! !


!InterpreterSimulator methodsFor: 'initialization' stamp: 'di 9/27/97 09:24'!
openOn: fileName extraMemory: extraBytes
	"InterpreterSimulator new openOn: 'clone.im' extraMemory: 100000"

	| f version headerSize count oldBaseAddr bytesToShift swapBytes |
	"open image file and read the header"
	checkAssertions _ false.
	f _ FileStream oldFileNamed: fileName.
	imageName _ f fullName.
	f binary; readOnly.
	version _ self nextLongFrom: f.  "current version: 16r1966 (=6502)"
	version = self imageFormatVersion
		ifTrue: [swapBytes _ false]
		ifFalse: [(version _ self byteSwapped: version) = self imageFormatVersion
					ifTrue: [swapBytes _ true]
					ifFalse: [self error: 'incomaptible image format']].
	headerSize _ self nextLongFrom: f swap: swapBytes.
	endOfMemory _ self nextLongFrom: f swap: swapBytes.  "first unused location in heap"
	oldBaseAddr _ self nextLongFrom: f swap: swapBytes.  "object memory base address of image"
	specialObjectsOop _ self nextLongFrom: f swap: swapBytes.
	lastHash _ self nextLongFrom: f swap: swapBytes.  "Should be loaded from, and saved to the image header"
	savedWindowSize _ self nextLongFrom: f swap: swapBytes.
	lastHash = 0 ifTrue: [lastHash _ 999].

	"allocate interpreter memory"
	memoryLimit _ endOfMemory + extraBytes.

	"read in the image in bulk, then swap the bytes if necessary"
	f position: headerSize.
	memory _ Bitmap new: memoryLimit // 4.
	count _ f readInto: memory startingAt: 1 count: endOfMemory // 4.
	count ~= (endOfMemory // 4) ifTrue: [self halt].
	f close.
	swapBytes ifTrue: [Utilities informUser: 'Swapping bytes of foreign image...'
								during: [self reverseBytesInImage]].

	self initialize.
	bytesToShift _ 0 - oldBaseAddr.  "adjust pointers for zero base address"
	endOfMemory _ endOfMemory.
	Utilities informUser: 'Relocating object pointers...'
				during: [self initializeInterpreter: bytesToShift].
	checkAssertions _ false.! !

!InterpreterSimulator methodsFor: 'testing' stamp: 'di 9/28/97 03:02'!
findNewMethodInClass: class
"
	| cName |
	traceOn ifTrue:
		[cName _ (self sizeBitsOf: class) = 16r20
			ifTrue: ['class ' , (self nameOfClass: (self fetchPointer: 6 ofObject: class))]
			ifFalse: [(self nameOfClass: class)].
		self cr; print: cName , '>>' , (self stringOf: messageSelector)].
"
"
(self stringOf: messageSelector) = 'raisedToInteger:' ifTrue: [self halt].
"
	sendCount _ sendCount + 1.
(false "sendCount > 1090" "and: [sendCount\\10 = 0]") ifTrue:
		[Transcript print: sendCount; space.
		self validate].
	super findNewMethodInClass: class.! !


!LookupKey methodsFor: 'comparing' stamp: 'di 9/27/97 20:45'!
identityHashMappedBy: map
	"Answer what my hash would be if oops changed according to map."

	^ key identityHashMappedBy: map! !


!SmallInteger methodsFor: 'comparing' stamp: 'di 9/27/97 20:32'!
identityHashMappedBy: map

	^ self! !


Object removeSelector: #basicHash!
SmallInteger removeSelector: #basicHash!
