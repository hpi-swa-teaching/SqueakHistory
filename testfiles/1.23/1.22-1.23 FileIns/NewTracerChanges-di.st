'From Squeak 1.22 of September 21, 1997 on 2 October 1997 at 12:35:09 am'!

!Bitmap methodsFor: 'filing' stamp: 'di 9/23/97 14:41'!
swapBytes
	self swapBytesFrom: 1 to: self size
! !

!Bitmap methodsFor: 'filing' stamp: 'di 10/2/97 00:02'!
swapBytesFrom: start to: stop
	"Perform a bigEndian/littleEndian byte reversal of my words"
	| hack blt |
	"The implementation is a hack, but fast for large ranges"
	hack _ Form new hackBits: self.
	blt _ (BitBlt toForm: hack) sourceForm: hack.
	blt combinationRule: Form reverse.  "XOR"
	blt sourceY: start-1; destY: start-1; height: stop-start+1; width: 1.
	blt sourceX: 0; destX: 3; copyBits.  "Exchange bytes 0 and 3"
	blt sourceX: 3; destX: 0; copyBits.
	blt sourceX: 0; destX: 3; copyBits.
	blt sourceX: 1; destX: 2; copyBits.  "Exchange bytes 1 and 2"
	blt sourceX: 2; destX: 1; copyBits.
	blt sourceX: 1; destX: 2; copyBits.
! !


!Form methodsFor: 'private' stamp: 'di 9/23/97 14:36'!
hackBits: bitThing
	"This method provides an initialization so that BitBlt may be used, eg, to 
	copy ByteArrays and other non-pointer objects efficiently.
	The resulting form looks 4 wide, 8 deep, and bitThing-size-in-words high."
	width _ 4.
	depth _ 8.
	bitThing class isBits ifFalse: [self error: 'bitThing must be a non-pointer object'].
	bitThing class isBytes
		ifTrue: [height _ bitThing size // 4]
		ifFalse: [height _ bitThing size].
	bits _ bitThing! !


!Interpreter methodsFor: 'image save/restore' stamp: 'di 9/23/97 15:22'!
byteSwapByteObjects
	"Byte-swap the words of all bytes objects in the image, including Strings, ByteArrays, and CompiledMethods. This returns these objects to their original byte ordering after blindly byte-swapping the entire image. For compiled methods, byte-swap only their bytecodes part."

	| oop fmt wordAddr methodHeader |
	oop _ self firstObject.
	[oop < endOfMemory] whileTrue: [
		(self isFreeObject: oop) ifFalse: [
			fmt _ self formatOf: oop.
			fmt >= 8 ifTrue: [  "oop contains bytes"
				wordAddr _ oop + BaseHeaderSize.
				fmt >= 12 ifTrue: [
					"compiled method; start after methodHeader and literals"
					methodHeader _ self longAt: oop + BaseHeaderSize.
					wordAddr _ wordAddr + 4 + (((methodHeader >> 10) bitAnd: 16rFF) * 4).
				].
				self reverseBytesFrom: wordAddr to: oop + (self sizeBitsOf: oop).
			].
 		].
		oop _ self objectAfter: oop.
	].
! !

!Interpreter methodsFor: 'image save/restore' stamp: 'di 9/23/97 15:20'!
reverseBytesFrom: startAddr to: stopAddr
	"Byte-swap the given range of memory (not inclusive!!)."
	| addr |
	addr _ startAddr.
	[addr < stopAddr] whileTrue:
		[self longAt: addr put: (self byteSwapped: (self longAt: addr)).
		addr _ addr + 4].! !

!Interpreter methodsFor: 'image save/restore' stamp: 'di 10/2/97 00:31'!
reverseBytesInImage
	"Byte-swap all words in memory after reading in the entire image file with bulk read. Contributed by Tim Rowledge."

	"First, byte-swap every word in the image. This fixes objects headers."
	self reverseBytesFrom: self startOfMemory to: endOfMemory.

	"Second, return the bytes of bytes-type objects to their orginal order."
	self byteSwapByteObjects.! !


!InterpreterSimulator methodsFor: 'initialization' stamp: 'di 9/23/97 15:51'!
nextLongFrom: aStream swap: swapFlag
	swapFlag 
		ifTrue: [^ self byteSwapped: (self nextLongFrom: aStream)]
		ifFalse: [^ self nextLongFrom: aStream]! !

!InterpreterSimulator methodsFor: 'initialization' stamp: 'di 10/2/97 00:32'!
reverseBytesFrom: begin to: end
	"Byte-swap the given range of memory (not inclusive!!)."
	| wordAddr |
	wordAddr _ begin.
	memory swapBytesFrom: wordAddr // 4 + 1 to: end // 4! !

!InterpreterSimulator methodsFor: 'testing' stamp: 'di 10/1/97 23:36'!
objectBefore: addr
	| oop prev |
	oop _ self firstObject.
	[oop < endOfMemory] whileTrue: [
		prev _ oop.  "look here if debugging prev obj overlapping this one"
		oop _ self objectAfter: oop.
		oop >= addr ifTrue: [^ prev]
	]! !

!InterpreterSimulator methodsFor: 'I/O primitives' stamp: 'di 9/23/97 15:26'!
showDisplayBits
	| displayObj destBits raster destDepth pixPerWord simDisp realDisp top bottom rect |
	displayObj _ self splObj: TheDisplay.
	self targetForm = displayObj ifFalse: [^ self].
	destBits _ self fetchPointer: 0 ofObject: displayObj.
	destDepth _ self fetchInteger: 3 ofObject: displayObj.
	pixPerWord _ 32 // destDepth.
	raster _ displayForm width + (pixPerWord - 1) // pixPerWord.
	simDisp _ Form new hackBits: memory.
	realDisp _ Form new hackBits: displayForm bits.
	top _ myBitBlt affectedTop.
	bottom _ myBitBlt affectedBottom.
	realDisp
		copy: (0 @ (top * raster) extent: 4 @ (bottom - top * raster))
		from: 0 @ (destBits + 4 // 4 + (top * raster))
		in: simDisp
		rule: Form over.
	rect _ 0 @ top corner: displayForm width @ bottom.
	Display
		copy: (rect translateBy: self displayLocation)
		from: rect topLeft
		in: displayForm
		rule: Form over! !


SystemTracer comment:
'NOTE:  The SystemTracer was invented by Ted Kaehler.  It has gone through many variations.  I have simplified it and adapted it to work with Squeak''s limited Object hashing.  It has written runnable Mac images on a Mac.  Some changes may be needed for it to produce runnables image on a bigEndian machine.  -- Dan I. 9/29/97

The SystemTracer runs through the system tracing every accessible object, and writing a copy of each object onto a disk image. It does this carefully enough that the resulting clone can be started up and run, just like a snapshot.  Great care must also be taken that the tracer never sees (due to clamping) any of the structures created dynamically while writing the clone.  If this happens an infinite recursion can result.
	
The tracer has built-in support for clamping unwanted (or unneeded) objects out of the system. It also allows fundamental changes to, eg, object header format, compiledMethod format, number formats, etc.  The current version is fairly simple, as it just copies what is already in the system. This version does, however, reassign object hashes in order to illustrate how one must remap all the hashed structures accordingly.

Squeak currently only has 12 bits of object hash.  A normal dictionary, used to map from old oop to new oop, would bog down in a system of, say, 50k objects.  This tracer has a special structure that is optimized for this purpose.  See the comment in initOopMap.  If we ever extend the object hash, this structure could be replaced by a simple IdentityDictionary.

When you first start up the cloned image, you will probably get a "cannotReturn" error, a vestige of the old tracing process.  This can simply be closed (or maybe you can figure out how to fix the tracers so it doens''t happen ;-).  '!

!SystemTracer methodsFor: 'initialization' stamp: 'di 9/29/97 09:09'!
writeImage: roots
	imageHeaderSize _ 64.	"16 longs"
	file position: imageHeaderSize.  "Skip header section"
	maxOop _ 0.  "Starting oop"
	self initCompactClasses.
	specialObjects _ Smalltalk specialObjectsArray copy.
	specialObjects at: 29 put: compactClasses.
	Transcript cr; show: 'write nil'.
	"New oop of nil is needed before we find out from the trace."
	NewNil _ maxOop + ((self headersFor: nil withHash: 0) size-1*4).
	self trace: nil.  "In fact, this traverses the system by the time it's done!!"
	Transcript cr; show: 'write specialObjects'.
	self trace: specialObjects.
	Transcript cr; show: 'write Smalltalk'.
	roots do: [:root | self trace: root].
	file position: maxOop + imageHeaderSize.		"End of all our objects"
	Transcript cr; show: 'writeHeader'.
	self writeFileHeader.
	^ Array with: maxOop! !

!SystemTracer methodsFor: 'tracing and writing' stamp: 'di 9/29/97 09:05'!
new: obj class: class length: length trace: traceBlock write: writeBlock
	| oop objpos headers type hash |
	"We assign file space for an object.
	Only does something when object has not been assigned a new oop yet.  
	Length is the instance vars and variable portion in longs.
	Class is not included in length.
	Special objects come here for an oop, and do no tracing or writing.
	To trace and write their fields later, do NOT use this method."  
	oop_ self mapAt: obj.
	oop = UnassignedOop ifFalse:
		["Has already been assigned a new oop or clamped."
		^ self].

	"Write header and remember new oop in map"
	hash _ (hashGenerator next * 16rFFF asFloat) asInteger.
	headers _ self headersFor: obj withHash: hash.
	file position: maxOop + imageHeaderSize.
	headers do: [:h | self write4Bytes: h].
	maxOop _ maxOop + (headers size-1*4).		"New oop points at header word"
	self mapAt: obj put: maxOop with: hash.
	objpos _ maxOop + imageHeaderSize.		"file position"
length > 20 ifTrue: [maxOop printString, ' ' displayAt: 0@0].

	"Write blank data, advancing to next object position"
	maxOop _ maxOop + (length+1*4).	"ready for next object"
	file nextPutAll: (ByteArray new: length*4 withAll: 0).

	traceBlock notNil ifTrue: [self trace: class.  traceBlock value].
	(headers size > 1 and: [(headers at: headers size-1) < 0])
		ifTrue: ["rewrite class word if not known before"
				file position: objpos-4.
				type _ (headers at: headers size-1) bitAnd: 3.
				self write4Bytes: (self mapAt: class) + type.
				self write4Bytes: (headers at: headers size) "faster to write than skip"]
		ifFalse: ["Had no class header, or was already valid"
				file position: objpos+4].
	"Now positioned after header, before data..."

	writeBlock value.	"No allocation of new oops is allowed in here!!"

	"Consistency check"
	file position = (objpos + (length+1*4)) ifFalse:
		["writeBlock did not leave us at end of object"
		self halt.
		"Maybe copied an object without putting it in holder,
		so it got freed and became something else of a different size"]! !

!SystemTracer methodsFor: 'private' stamp: 'di 9/29/97 08:48'!
headersFor: obj withHash: hash
	"Create three header words for this object.  Length, class, header bits."
	"Three possibilities:
		Length, class, header bits
		0, class, header bits
		0,0, header bits"
	| header3 header2 header1 cix sizeFld |
	"3 gc bits"
	header1 _ 0.  "Mark, old, dirty"

	header1 _ header1 bitShift: 12.	"next fld is 12 bits"
	header1 _ header1 + (hash bitAnd: 16rFFF).

	header1 _ header1 bitShift: 5.
	sizeFld _ (self sizeInWordsOf: obj) + 1.	"size in long words, incl hdr0"
	cix _ compactClasses indexOf: obj class.	"0 means need full word"
	header2 _ self mapAt: obj class.
	header1 _ header1 + (cix bitAnd: 16r1F).

	header1 _ header1 bitShift: 4.
	header1 _ header1 + (self formatOf: obj).	"Class characteristics"

	header1 _ header1 bitShift: 6.
	sizeFld > 16r3F
		ifTrue: [header3 _ sizeFld bitShift: 2.
				sizeFld _ 0]
		ifFalse: [header3 _ 0].
	header1 _ header1 + sizeFld.

	header1 _ header1 bitShift: 2.

	header3 > 0 ifTrue:
		["3-word: type=0"
		^ Array with: header3+0 with: header2+0 with: header1+0].
	cix = 0 ifTrue:
		[ "2-word: type=1"
		^ Array with: header2+1 with: header1+1].
	"1-word: type=3"
	^ Array with: header1+3! !


