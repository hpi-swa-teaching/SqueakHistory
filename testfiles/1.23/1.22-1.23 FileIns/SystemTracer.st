'From Squeak 1.22 of September 21, 1997 on 29 September 1997 at 9:21:23 am'!

SystemTracer removeFromSystem!

Object subclass: #SystemTracer
	instanceVariableNames: 'oopMap map file writeDict maxOop specialObjects initialProcess hashGenerator imageHeaderSize cleaningUp compactClasses '
	classVariableNames: 'Clamped NewNil UnassignedOop '
	poolDictionaries: ''
	category: 'Squeak Interpreter'!
SystemTracer comment:
'NOTE:  The SystemTracer was invented by Ted Kaehler.  It has gone through many variations.  I have simplified it and adapted it to work with Squeak''s limited Object hashing.  It has written runnable Mac images on a Mac.  Thanks to Paul McCullough for help with this.  Some changes may be needed for it to produce runnables image on a bigEndian machine.  -- Dan I. 9/29/97

The SystemTracer runs through the system tracing every accessible object, and writing a copy of each object onto a disk image. It does this carefully enough that the resulting clone can be started up and run, just like a snapshot.  Great care must also be taken that the tracer never sees (due to clamping) any of the structures created dynamically while writing the clone.  If this happens an infinite recursion can result.
	
The tracer has built-in support for clamping unwanted (or unneeded) objects out of the system. It also allows fundamental changes to, eg, object header format, compiledMethod format, number formats, etc.  The current version is fairly simple, as it just copies what is already in the system. This version does, however, reassign object hashes in order to illustrate how one must remap all the hashed structures accordingly.

Squeak currently only has 12 bits of object hash.  A normal dictionary, used to map from old oop to new oop, would bog down in a system of, say, 50k objects.  This tracer has a special structure that is optimized for this purpose.  See the comment in initOopMap.  If we ever extend the object hash, this structure could be replaced by a simple IdentityDictionary.

When you first start up the cloned image, you will probably get a "cannotReturn" error, a vestige of the old tracing process.  This can simply be closed (or maybe you can figure out how to fix the tracer so it doens''t happen ;-).  '!

!SystemTracer methodsFor: 'initialization' stamp: 'di 9/28/97 16:13'!
doit  "SystemTracer writeClone"
	"(StandardFileStream allInstances select:
		[:f | f name endsWith: 'clone.image']) do: [:f | f close]."
	| time1 time2 ctxt n |
	ctxt _ thisContext sender.
	ctxt push: nil.
	self init: ctxt.
	cleaningUp _ false.	"true means rewriting special objects in writeSpecial2"
	Transcript show: 'Tracing . . . '.
	time1 _ Time millisecondClockValue.
	file _ FileStream fileNamed: 'clone.image'.
	file binary.
	n _ self writeImage: (Array with: Smalltalk).
	time2 _ Time millisecondClockValue.
	Transcript cr; show: n printString , ' bytes written in '
			, (time2 - time1 //1000) printString , ' seconds.'.
	"ctxt pop" "So we can resume"! !

!SystemTracer methodsFor: 'initialization' stamp: 'di 9/27/97 22:27'!
init: aContext
	initialProcess _ Process forContext: aContext priority: Processor activePriority.
	self initOopMap.
	hashGenerator _ Random new.
	self clamp: self.
	self clamp: aContext sender.
	self initDict! !

!SystemTracer methodsFor: 'initialization' stamp: 'di 9/27/97 11:29'!
initCompactClasses
	| c |
	c _ Array new: 31.	
	"These classes have a short name (their index in this table.  It is not their oop.)
	Thus their instances can use just a single word as their header in memory."
	c at: 1 put: CompiledMethod.  c at: 2 put: Symbol. c at: 3 put: Array.
	c at: 4 put: Float.  c at: 5 put: LargePositiveInteger.  c at: 6 put: String.
	c at: 7 put: MethodDictionary.  c at: 8 put: Association.  c at: 9 put: Point.
	c at: 10 put: Rectangle.  c at: 11 put: ClassOrganizer.  c at: 12 put: TextLineInterval.
	c at: 13 put: BlockContext.  c at: 14 put: MethodContext.  c at: 15 put: nil.
	compactClasses _ c.
	"Leave 16 to 31 for user defined compact classes."! !

!SystemTracer methodsFor: 'initialization' stamp: 'di 9/28/97 00:22'!
initDict
	writeDict _ Dictionary new: 256.
	Smalltalk allClassesDo: 
		[:class | 
		class isBits 
			ifTrue: 
			[writeDict at: class put: (class isBytes ifTrue: [#writeBytes:]
												ifFalse: [#writeWords:])]
			ifFalse:
			[writeDict at: class put: #writePointers:.
			(class inheritsFrom: Set) | (class == Set) ifTrue:
				[writeDict at: class put: #writeSet:].
			(class inheritsFrom: IdentitySet) | (class == IdentitySet) ifTrue:
				[writeDict at: class put: #writeIdentitySet:].
			(class inheritsFrom: IdentityDictionary) | (class == IdentityDictionary) ifTrue:
				[writeDict at: class put: #writeIdentitySet:].
			(class inheritsFrom: MethodDictionary) | (class == MethodDictionary) ifTrue:
				[writeDict at: class put: #writeMethodDictionary:]].
				].
	Smalltalk allBehaviorsDo: 
		[:class | writeDict at: class class put: #writeBehavior:].
	writeDict at: SmallInteger put: #writeClamped:.
	writeDict at: CompiledMethod put: #writeMethod:.
	writeDict at: Process put: #writeProcess:.
	writeDict at: MethodContext put: #writeContext:.
	writeDict at: BlockContext put: #writeContext:.! !

!SystemTracer methodsFor: 'initialization' stamp: 'di 9/28/97 15:02'!
writeFileHeader
	| dataLen |
	dataLen _ file position - imageHeaderSize.
	file position: 0.  "info in header page"
	self write4Bytes: ($A asciiValue *100) + 2.  "version number:  6500+2"
	self write4Bytes: imageHeaderSize.  "File offset (bytes) of start of data"
							"same as base address (byte) of first object"
	self write4Bytes: dataLen.  "Length of data segment in words"
	self write4Bytes: 0.		"what you have to add to an oop to get"
							"an offset in the data portion of this file"
	self write4Bytes: (self mapAt: specialObjects).
	self write4Bytes: (hashGenerator next * 16rFFF asFloat) asInteger.  "next hash"
	self write4Bytes: Display width * 16r10000 + Display height.  "display size"
	file position > imageHeaderSize ifTrue: [self error: 'Header ran over allotted length'].
	file padTo: imageHeaderSize put: 0.  "Pad header page"
	file close! !

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


!SystemTracer methodsFor: 'mapping oops' stamp: 'di 9/29/97 08:21'!
clamp: obj
	self mapAt: obj put: Clamped with: nil! !

!SystemTracer methodsFor: 'mapping oops'!
hasClamped: obj
	"See if obj will be a SmallInteger in the new system."
	obj class == SmallInteger ifTrue: [^ true].
	^ (self mapAt: obj) = Clamped! !

!SystemTracer methodsFor: 'mapping oops'!
initOopMap
	"oopMap is an array 4096 long indexed by basicHash.
	Each element a subarray of object/newOop/hash triplets.
	The subarrrays must be linearly searched.
	Access to an object causes it to be promoted in the subarray,
		so that frequently accessed objects can be found quickly."
	oopMap _ (1 to: 4096) collect: [:i | Array new].! !

!SystemTracer methodsFor: 'mapping oops' stamp: 'di 9/27/97 20:26'!
mapAt: obj
	"Return the new oop for this object"
	| bucket |
	bucket _ oopMap at: obj identityHash+1.
	1 to: bucket size by: 3 do: 
		[:i | obj == (bucket at: i)
			ifTrue: ["Promote this entry for rapid access"
					i > 1 ifTrue: [1 to: 3 do: [:j | bucket swap: j with: i-1+j]].
					^ bucket at: 2]].
	^ UnassignedOop! !

!SystemTracer methodsFor: 'mapping oops' stamp: 'di 9/27/97 20:26'!
mapAt: obj put: oop with: hash
	"Assign the new oop for this object"
	| bucket |
	bucket _ oopMap at: obj identityHash+1.

	"Check for multiple writes (debug only)"
"	1 to: bucket size by: 3 do: 
		[:i | obj == (bucket at: i) ifTrue: [self halt]].
"
	oopMap at: obj identityHash+1 put: (Array with: obj with: oop with: hash) , bucket! !

!SystemTracer methodsFor: 'mapping oops' stamp: 'di 9/27/97 20:27'!
mapHashAt: obj
	"Return the new hash for this object"
	| bucket |
	bucket _ oopMap at: obj identityHash+1.
	1 to: bucket size by: 3 do: 
		[:i | obj == (bucket at: i) ifTrue: [^ bucket at: 3]].
	self halt! !


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

!SystemTracer methodsFor: 'tracing and writing'!
trace: obj

	(self hasClamped: obj) ifTrue: [^ self].
	self perform: (writeDict at: obj class ifAbsent: [#writeClamped:])
			"May be some classes not in Smalltalk dict, let them through"
		with: obj.! !

!SystemTracer methodsFor: 'tracing and writing' stamp: 'di 9/28/97 14:40'!
writeBehavior: obj
	| length |
	self new: obj
		class: obj class
		length: (length _ self sizeInWordsOf: obj)
		trace: [1 to: length do: [:i | self trace: (obj instVarAt: i)]]
		write: [1 to: 2 do: [:i | self writePointerField: (obj instVarAt: i)].
			self writePointerField: (self formatOfCls: obj).
			4 to: length do: [:i | self writePointerField: (obj instVarAt: i)]]! !

!SystemTracer methodsFor: 'tracing and writing'!
writeBytes: obj

	self new: obj
		class: obj class
		length: (self sizeInWordsOf: obj)
		trace: []
		write: 
			[1 to: obj size do: [:i | file nextPut: (obj at: i) asInteger].
			file padToNextLongPut: 0]! !

!SystemTracer methodsFor: 'tracing and writing'!
writeClamped: obj

	^false! !

!SystemTracer methodsFor: 'tracing and writing' stamp: 'di 9/28/97 01:33'!
writeContext: obj
	"Nil out any garbage above the stack pointer to avoid a crash."
	obj stackPtr == nil ifFalse:
		[obj stackPtr+1 to: obj size do: [:ind | obj at: ind put: nil]].
	^ self writePointers: obj	"Normal Case"! !

!SystemTracer methodsFor: 'tracing and writing' stamp: 'di 9/28/97 00:20'!
writeHashArray: obj permutedBy: perm
	"Elements of a Set's hashed array need to be reordered according to perm."
	| length |
	self new: obj class: obj class length: (length _ self sizeInWordsOf: obj)
		trace:	[1 to: length do:
					[:i | self trace: (obj basicAt: i)]]
		write:	[1 to: length do:
					[:i | self writePointerField: ((perm at: i) == nil
										ifTrue: [nil]
										ifFalse: [obj basicAt: (perm at: i)])]]! !

!SystemTracer methodsFor: 'tracing and writing' stamp: 'di 9/28/97 00:15'!
writeHashArrayPermuted: obj useIdentity: useIdentity
	"Elements of a Set's hashed array need to be reordered according to new oops."
	| length perm |
	self new: obj class: obj class length: (length _ self sizeInWordsOf: obj)
		trace:	[1 to: length do: [:i | self trace: (obj basicAt: i)].
				"Now get permutation based on new oops"
				perm _ self permutationFor: obj useIdentity: useIdentity]
		write:	[1 to: length do:
					[:i | self writePointerField: ((perm at: i) == nil
										ifTrue: [nil]
										ifFalse: [obj basicAt: (perm at: i)])]]! !

!SystemTracer methodsFor: 'tracing and writing' stamp: 'di 9/27/97 20:44'!
writeIdentitySet: obj 
	"Elements of a Set need to be reordered owing to new oops."
	^ self writeSet: obj useIdentity: true! !

!SystemTracer methodsFor: 'tracing and writing'!
writeMethod: obj
	| nptrs |
	nptrs _ obj numLiterals + 1.
	self new: obj
		class: obj class
		length: (self sizeInWordsOf: obj)
		trace: [2 to: nptrs do: [:i | self trace: (obj objectAt: i)]]
		write: 
			[self writePointerField: (self methodHeader: obj).
			2 to: nptrs do: [:i | self writePointerField: (obj objectAt: i)].
			nptrs * 4 + 1 to: obj size do: [:i | file nextPut: (obj at: i)].
			file padToNextLongPut: 0]! !

!SystemTracer methodsFor: 'tracing and writing' stamp: 'di 9/28/97 00:22'!
writeMethodDictionary: obj
	"Elements of a Set need to be reordered according to new oops."
	| perm |
	self new: obj class: obj class length: (self sizeInWordsOf: obj)
		trace:	["First need to map the indexable fields (selectors)"
				1 to: obj basicSize do: [:i | self trace: (obj basicAt: i)].
				"Now get permutation based on new oops"
				perm _ self permutationFor: obj useIdentity: true.
				"Map named inst vars *assuming* 2nd is the hash array"
				1 to: obj class instSize do:
					[:i | i=2 ifTrue: ["Permute the hash array and note its permutation"
									self writeHashArray: (obj instVarAt: i) permutedBy: perm]
							ifFalse: ["Other fields get traced normally"
									self trace: (obj instVarAt: i)]]]
		write:	[1 to: obj class instSize do:
					[:i | self writePointerField: (obj instVarAt: i)].
				1 to: obj basicSize do:
					[:i | self writePointerField: ((perm at: i) == nil
											ifTrue: [nil]
											ifFalse: [obj basicAt: (perm at: i)])]]! !

!SystemTracer methodsFor: 'tracing and writing'!
writePointers: obj
	| length |
	self new: obj
		class: obj class
		length: (length _ self sizeInWordsOf: obj)
		trace: [1 to: length do: [:i | self trace: (obj instVarAt: i)]]
		write: [1 to: length do: [:i | self writePointerField: (obj instVarAt: i)]]! !

!SystemTracer methodsFor: 'tracing and writing'!
writeProcess: obj 
	"Substitute new initialProcess for current."
	| newObj instSize |
	newObj _ obj == Processor activeProcess
			ifTrue: [initialProcess]
			ifFalse: [obj].
	self new: obj
		class: newObj class
		length: (instSize _ newObj class instSize)
		trace: [1 to: instSize do:
				[:i | self trace: (newObj instVarAt: i)]]
		write: [1 to: instSize do:
				[:i | self writePointerField: (newObj instVarAt: i)]]! !

!SystemTracer methodsFor: 'tracing and writing' stamp: 'di 9/27/97 20:43'!
writeSet: obj 
	"Elements of a Set need to be reordered owing to new oops."
	^ self writeSet: obj useIdentity: false! !

!SystemTracer methodsFor: 'tracing and writing' stamp: 'di 9/28/97 00:01'!
writeSet: obj useIdentity: useIdentity
	"Elements of a Set need to be reordered according to new oops."
	| |
	self basicSize > 0 ifTrue: [self halt. "Not clear how to permute this kind of set"].
	self new: obj class: obj class length: (self sizeInWordsOf: obj)
		trace: 
			["Map named inst vars *assuming* 2nd is the hash array"
			1 to: obj class instSize do:
				[:i | i=2 ifTrue: ["Permute the hash array and note its permutation"
								self writeHashArrayPermuted: (obj instVarAt: i)
											useIdentity: useIdentity]
						ifFalse: ["Other fields get traced normally"
								self trace: (obj instVarAt: i)]]]
		write: 
			[1 to: obj class instSize do:
				[:i | self writePointerField: (obj instVarAt: i)]]! !

!SystemTracer methodsFor: 'tracing and writing'!
writeWords: obj

	self new: obj
		class: obj class
		length: (self sizeInWordsOf: obj)
		trace: []
		write: [1 to: obj basicSize do: [:i | self write4Bytes: (obj instVarAt: i)]]! !


!SystemTracer methodsFor: 'private' stamp: 'di 9/28/97 14:46'!
formatOf: obj
	"Make the code that tells the format of this object.
	It is like the class's instSpec, but with added low bits for byte size"
"       0=      No pointer fields
        1=      Fixed pointer fields only
        2=      Var pointer fields only
        3=      Fixed and var pointer fields
 
        4=      unused
        5=      unused
        6=      var long (bit) fields only
        7=      unused
 
        8-11=   var byte fields only
                low 2 bits are low 2 bits of size **
        12-15   methods -- ie #literals in header, followed by var bytes
                same interpretation of low 2 bits"
	| class spec |
	class _ obj class.
	spec _ class instSpec.    "just use what's there"
	spec < 8 ifTrue: [^ spec]
			ifFalse: ["For byte objects, size = wordSize - spec.lowBits"
					^ spec + (3 - (obj size+3 bitAnd: 3))]! !

!SystemTracer methodsFor: 'private' stamp: 'di 9/28/97 14:46'!
formatOfCls: class
	"Return the full word value that encodes instSize, bits, bytes, and variable."
	"See the comment in Behavior format:variable:words:pointers:"
	^ class format    "just use what's there"! !

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

!SystemTracer methodsFor: 'private' stamp: 'di 9/28/97 14:45'!
methodHeader: obj
	"Return the integer encoding the attributes of this method"
	"See the comment in CompiledMethod newBytes:nArgs:nTemps:nStack:nLits:primitive:"
	^ obj header  "just use what's there"! !

!SystemTracer methodsFor: 'private' stamp: 'di 9/28/97 14:47'!
newHashFor: obj
	"If an object has a hash derived from its value, it will override on the way here.
	This object can use anything as a hash.  Derive one from its oop."

	(self mapAt: obj) = UnassignedOop
		ifTrue: [self halt]
		ifFalse: [^ self mapHashAt: obj]  ! !

!SystemTracer methodsFor: 'private' stamp: 'di 9/27/97 20:40'!
permutationFor: array useIdentity: useIdentity
	"Return an inverse permutation for an array to permute it according to
	the mapped oop values. The keys in array MUST have been mapped."
	| len perm key hash |
	len _ array basicSize.  
	perm _ Array new: len.
	1 to: len do:
		[:i | key _ array basicAt: i.
		(key == nil or: [self hasClamped: key])
		  ifFalse:
			[hash _ useIdentity
					ifTrue: [key identityHashMappedBy: self]
					ifFalse: [key hashMappedBy: self].
			hash _ hash \\ len + 1.
			[(perm at: hash) == nil] 
				whileFalse:
				[hash _ (hash = len ifTrue: [1] ifFalse: [hash + 1])].
			perm at: hash put: i]].
	^ perm! !

!SystemTracer methodsFor: 'private'!
sizeInWordsOf: anObject
"NOTE: This is the new length of the object in LONG WORDS.
		Does not include the class (header) word."
	| class |
	class _ anObject class.
	class isBytes ifTrue: [^ anObject basicSize+3 // 4].
	class isBits ifTrue: [^ anObject basicSize].	"in two byte chunks"
	class isVariable ifTrue: [^ class instSize + anObject basicSize].
	^ class instSize! !

!SystemTracer methodsFor: 'private' stamp: 'di 9/28/97 14:20'!
write4Bytes: bits 
	"Avoid overhead of large integers and nextWord:put:."
	| posBits bytes |
	bits positive
		ifTrue: [posBits _ bits]
		ifFalse: ["Change rep to twos complement."
				posBits _ 16rFFFFFFFF+(bits+1)].
	bytes _ ByteArray new: 4.
	bytes at: 1 put: (posBits digitAt: 4).
	bytes at: 2 put: (posBits digitAt: 3).
	bytes at: 3 put: (posBits digitAt: 2).
	bytes at: 4 put: (posBits digitAt: 1).
	file nextPutAll: bytes! !

!SystemTracer methodsFor: 'private' stamp: 'di 9/28/97 12:03'!
writePointerField: obj 
	| newOop |
	obj class == SmallInteger ifTrue: 
		[obj >= 0 ifTrue: [newOop _ obj * 2 + 1]
				ifFalse: [newOop _ (16r80000000 + obj) * 2 + 1].
		self write4Bytes: newOop.
		^ obj].		

	"normal pointers"
	(newOop _ self mapAt: obj) = Clamped
		ifTrue: ["If object in this field is not being traced, put out nil."
				self write4Bytes: NewNil]
		ifFalse: [self write4Bytes: newOop]! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

SystemTracer class
	instanceVariableNames: ''!

!SystemTracer class methodsFor: 'instance creation' stamp: 'di 9/27/97 14:01'!
initialize    "SystemTracer initialize"
	"These consts are negative, so they will not match any oop.
	It is important, though, that UnassignedOop, at least, have
	zero in its low-order 2 bits, (like all oops) so that the use of +
	to merge the header type bits (happens in new:class:...) will
	not do weird things."
	Clamped _ -4.  "Flag clamped objects in oopMap"	
	UnassignedOop _ -8.  "Flag unassigned oops in oopMap"! !

!SystemTracer class methodsFor: 'instance creation' stamp: 'di 9/27/97 12:30'!
writeClone  "SystemTracer writeClone"
	| tracer |
	tracer _ self new.
	"Delay shutDown."  "part of Smalltalk processShutDownList."
	tracer doit.   " <-- execution in clone resumes after this send"
	tracer == nil "will be nil in clone, since it is clamped"
		ifTrue: [Smalltalk processStartUpList].
	^ tracer! !


SystemTracer initialize!
