Object variableByteSubclass: #CFunction
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'DLL callout'!

!CFunction methodsFor: 'ES compatibility'!
address: a32bitInteger
	^self uint32At: 0 put: a32bitInteger
! !

!CFunction methodsFor: 'ES compatibility'!
equals: anObject
	^self error: 'This is not ES'
! !

!CFunction methodsFor: 'ES compatibility'!
functionType
	self error: 'this is not ES'
! !

!CFunction methodsFor: 'ES compatibility'!
isIdenticalLiteral: literal
	self error: 'this is not ES'
! !

!CFunction methodsFor: 'ES compatibility'!
isInLibrary
	^true! !

!CFunction methodsFor: 'ES compatibility'!
isNamed
	"Answer true if the receiver is identified by name,
	 and false if it's identified by ordinal number"

	^(self flags bitAnd: 1) == 1
! !

!CFunction methodsFor: 'ES compatibility'!
language
	"In fact, this function is not needed:
	 unlike ES, this implementation of DLL support supports only C language with calling convention System."
	^#c
! !

!CFunction methodsFor: 'ES compatibility'!
parameterCount
	^self flags >> 4 bitAnd: 15
! !

!CFunction methodsFor: 'ES compatibility'!
parameterTypeAt: index
	"index is 1-based"
	^CFunction conversionTypeSymbolFor: (self parameterTypeCodeAt: index)
! !

!CFunction methodsFor: 'ES compatibility'!
parameterTypeAt: index put: symbol
	"index is 1-based"
	self parameterTypeCodeAt: index put: (CFunction conversionTypeCodeFor: symbol)

! !

!CFunction methodsFor: 'ES compatibility'!
parameterTypeCodeAt: index
	"index is 1-based"
	^self at: index+9
! !

!CFunction methodsFor: 'ES compatibility'!
parameterTypeCodeAt: index put: code
	"index is 1-based"
	self at: index+9 put: code
! !

!CFunction methodsFor: 'ES compatibility'!
parameterTypeCodes
	"Return an Array of parameter type codes"
	| codes |
	codes := Array new: self parameterCount.
	1 to: self parameterCount do: [:parmIndex | codes at: parmIndex put: (self parameterTypeCodeAt: parmIndex)].
	^ codes! !

!CFunction methodsFor: 'ES compatibility'!
parameterTypeCodes: anArrayOfCodes

	anArrayOfCodes size = self parameterCount
		ifFalse: [^self error: 'Wrong number of parameters'].

	1 to: self parameterCount do: [:parmIndex |
		self parameterTypeCodeAt: parmIndex put: (anArrayOfCodes at: parmIndex)].
! !

!CFunction methodsFor: 'ES compatibility'!
parameterTypes: anArrayOfSymbols

	| codes |
	codes := anArrayOfSymbols collect: [ :aSymbol | CFunction conversionTypeCodeFor: aSymbol ].
	self parameterTypeCodes: codes


! !

!CFunction methodsFor: 'ES compatibility'!
printOn: aStream

	aStream
		nextPut: $<;
		nextPutAll: self returnType printString;
		nextPutAll: '=[';
		nextPutAll: self hModule printString;
		nextPutAll: ']:' .
	self isNamed ifTrue: [
		aStream nextPutAll: self name
	] ifFalse: [
		aStream nextPutAll: self functionNumber printString
	].
	aStream nextPutAll: ' ('.
	self parameterTypes do: [:x | aStream nextPutAll: x printString, ' ' ].
	aStream nextPutAll: ')>'.
! !

!CFunction methodsFor: 'ES compatibility'!
returnType: aSymbol
	self returnTypeCode: (CFunction conversionTypeCodeFor: aSymbol).
! !

!CFunction methodsFor: 'ES compatibility'!
returnTypeCode

	^self flags >> 1 bitAnd: 7
! !

!CFunction methodsFor: 'ES compatibility'!
returnTypeCode: value
	| bits |
	bits := value << 1.
	^self flags: (self flags bitAnd: 2r11110001)+bits
! !

!CFunction methodsFor: 'ES compatibility'!
unbind
	"This doesn't mean any platform action.
	 What this is meant for is to tell to the other methods when we somehow know that the address is invalid"
	self address: 0
! !


!CFunction methodsFor: 'Object memory access'!
oop32At: index
	<primitive: 1532>
	^self primitiveFailed
! !

!CFunction methodsFor: 'Object memory access'!
oop32At: index put: uint
	<primitive: 1533>
	^self primitiveFailed
! !

!CFunction methodsFor: 'Object memory access'!
uint32At: index
	<primitive: 1530>
	^self primitiveFailed
! !

!CFunction methodsFor: 'Object memory access'!
uint32At: index put: uint
	<primitive: 1531>
	^self primitiveFailed
! !


!CFunction methodsFor: 'public--calling'!
call
	<primitive: 1500>
	^self primitiveFailed
! !

!CFunction methodsFor: 'public--calling'!
callWith: arg1
	<primitive: 1501>
	^self primitiveFailed
! !

!CFunction methodsFor: 'public--calling'!
callWith: arg1 with: arg2
	<primitive: 1502>
	^self primitiveFailed
! !

!CFunction methodsFor: 'public--calling'!
callWith: arg1 with: arg2 with: arg3
	<primitive: 1503>
	^self primitiveFailed
! !

!CFunction methodsFor: 'public--calling'!
callWith: arg1 with: arg2 with: arg3 with: arg4
	<primitive: 1504>
	^self primitiveFailed
! !

!CFunction methodsFor: 'public--calling'!
callWith: arg1 with: arg2 with: arg3 with: arg4 with: arg5
	<primitive: 1505>
	^self primitiveFailed
! !

!CFunction methodsFor: 'public--calling'!
callWith: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6
	<primitive: 1506>
	^self primitiveFailed
! !

!CFunction methodsFor: 'public--calling'!
callWith: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6 with: arg7
	<primitive: 1507>
	^self primitiveFailed
! !


!CFunction methodsFor: 'private'!
bind
	"Bind the function by calling DosQueryProcAddr()"
	<primitive: 1523>
	^ self primitiveFailed! !

!CFunction methodsFor: 'private'!
flags
	^self at: 9! !

!CFunction methodsFor: 'private'!
flags: fl
	^self at: 9 put: fl! !

!CFunction methodsFor: 'private'!
functionNumber: n

	self isNamed ifTrue: [^self error].
	^self uint32At: 24 put: n
! !

!CFunction methodsFor: 'private'!
hModule: h
	self uint32At: 4 put: h; bind
! !

!CFunction methodsFor: 'private'!
hModuleIsValid
	^self hModule ~= 0
! !

!CFunction methodsFor: 'private'!
isNamed: bool
	bool ifTrue: [ self flags: (self flags bitOr: 1) ]
		ifFalse: [ self flags: (self flags bitAnd: 2r11111110) ]
! !

!CFunction methodsFor: 'private'!
name: aString

	self isNamed ifFalse: [^self error].
	^self oop32At: 24 put: aString
! !

!CFunction methodsFor: 'private'!
parameterCount: n
	| bits |
	bits := n<<4.
	^self flags: (self flags bitAnd: 2r00001111)+bits
! !


!CFunction methodsFor: 'public'!
address
	"Answer a 32 bit integer representing the address of the function.
	 If the function is not bound, its address is 0."

	^self uint32At: 0
! !

!CFunction methodsFor: 'public'!
functionNumber

	self isNamed ifTrue: [^nil].
	^self uint32At: 24
! !

!CFunction methodsFor: 'public'!
hModule

	^self uint32At: 4
! !

!CFunction methodsFor: 'public'!
name

	self isNamed ifFalse: [^nil].
	^self oop32At: 24
! !

!CFunction methodsFor: 'public'!
parameterTypes
	"Return an Array of parameter type symbols"
	"There is no such method in ES"
	| codes |
	codes := Array new: self parameterCount.
	1 to: self parameterCount do: [:parmIndex | codes at: parmIndex put: (self parameterTypeAt: parmIndex)].
	^ codes! !

!CFunction methodsFor: 'public'!
returnType
	^CFunction conversionTypeSymbolFor: self returnTypeCode! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

CFunction class
	instanceVariableNames: ''!

!CFunction class methodsFor: 'ES compatibility'!
unbindAll
	CFunction allInstances do: [ :f | f unbind; hModule: 0 ].
! !


!CFunction class methodsFor: 'type tables'!
conversionTypeCodeFor: aSymbol 
	^ self conversionTypes indexOf: aSymbol ifAbsent:
			[self error: 'unknown conversion type ' , aSymbol printString]

! !

!CFunction class methodsFor: 'type tables'!
conversionTypes
	^#(void uint bool object string bytes char )
! !

!CFunction class methodsFor: 'type tables'!
conversionTypeSymbolFor: anInteger 
	(anInteger > self conversionTypes size or: [anInteger <= 0])
		ifTrue: [^self error: 'wrong type code ' , anInteger printString].
	^self conversionTypes at: anInteger

! !

!CFunction class methodsFor: 'type tables'!
isValidConversionType: aSymbol
	^self error: 'still not implemented'
! !


!CFunction class methodsFor: 'shutdown'!
imageShutDown
	self unbindAll
! !


!CFunction class methodsFor: 'public'!
function: f dll: dll parameterTypes: pt returnType: rt 
	"This works for both ordinal-numbered and named functions, 
	 and is the way you create C Functions (to add them to 
	 the FunctionDictionary)"
	| instance |
	instance := self new: 32.

	f isInteger ifTrue: [	"The function is identified by ordinal number"
		instance isNamed: false.	"Order of these two lines is important (see #functionNumber:)"
		instance functionNumber: f
	] ifFalse: [	"The function is identified by name"
		instance isNamed: true.
		instance name: f
	].

	instance unbind.
	instance hModule: dll hModule.

	instance returnType: rt.
	instance parameterCount: pt size.
	instance parameterTypes: pt.

	^instance
! !


Object subclass: #DLL
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'DLL callout'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

DLL class
	instanceVariableNames: 'hModule '!

!DLL class methodsFor: 'private'!
adjustDictionary
"In the FunctionDictionary, there are functions who
 belong to this DLL.  Now that we have just loaded the
 DLL and have the hModule, we can tell all functions
 what the hModule is"
	self FunctionDictionary do: [ :f | f hModule: hModule ]
! !

!DLL class methodsFor: 'private'!
hModule
	hModule isNil ifTrue: [self load].
	^hModule
! !

!DLL class methodsFor: 'private'!
notifyDictionaryAboutUnload

	self FunctionDictionary do: [ :f | f unbind ]
! !

!DLL class methodsFor: 'private'!
primLoad: moduleName
		<primitive: 1520>
		^self primitiveFailed
! !

!DLL class methodsFor: 'private'!
primUnload
		<primitive: 1521>
		^self primitiveFailed
! !


!DLL class methodsFor: 'public'!
FunctionDictionary
	"Please override this method in your concrete class"
	^self subclassResponsibility
! !

!DLL class methodsFor: 'public'!
load
	hModule := self primLoad: self moduleName.
		"This loads the module, giving its handle"
	self adjustDictionary.
	^hModule! !

!DLL class methodsFor: 'public'!
moduleName
	"Please override this method in your DLL class"
	^self subclassResponsibility
! !

!DLL class methodsFor: 'public'!
unload
	self primUnload; notifyDictionaryAboutUnload.
! !
