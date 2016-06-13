'From Squeak 1.22 of September 21, 1997 on 4 October 1997 at 10:27:55 am'!

!ControlManager methodsFor: 'accessing' stamp: 'di 10/4/97 09:05'!
scheduledWindowControllers
	"Same as scheduled controllers, but without ScreenController.
	Avoids null views just after closing, eg, a debugger."

	^ scheduledControllers select:
		[:c | c ~~ screenController and: [c view ~~ nil]]! !

!ControlManager methodsFor: 'displaying' stamp: 'di 10/4/97 09:03'!
restore
	"Clear the screen to gray and then redisplay all the scheduled views.  Try to be a bit intelligent about the view that wants control and not display it twice if possible."

	scheduledControllers first view uncacheBits.  "assure refresh"
	self unschedule: screenController; scheduleOnBottom: screenController.
	screenController view window: Display boundingBox; displayDeEmphasized.
	self scheduledWindowControllers reverseDo:
		[:aController | aController view displayDeEmphasized].
! !


!StandardFileStream class methodsFor: 'file creation' stamp: 'di 10/4/97 10:09'!
newFileNamed: aFileName
 	"create a file in the default directory (or in the directory contained in the input arg), set for write access."
	| result selection |
	(self isAFileNamed: aFileName) ifFalse:
		[^ self new open: aFileName forWrite: true].

	"File already exists..."
	selection _ (PopUpMenu labels: 'overwrite that file
choose another name
cancel')
			startUpWithCaption: (self localNameFor: aFileName) , '
already exists.'.
	selection = 1 ifTrue:
		[result _ FileDirectory default deleteFileNamed: aFileName.
		result == nil ifTrue: "deletion failed"
				[self halt: 'Sorry - deletion failed'].
		^ self new open: aFileName forWrite: true].
	selection = 2 ifTrue:
		[^ self newFileNamed:
			(FillInTheBlank request: 'Enter a new file name'
						initialAnswer: aFileName)].
	self halt! !

!StandardFileStream class methodsFor: 'file creation' stamp: 'di 10/4/97 10:16'!
oldFileNamed: aFileName 
 	"Open a file in the default directory (or in the directory contained
	in the input arg); by default, it's available for reading.  2/12/96 sw
	Prior contents will be overwritten, but not truncated on close.  3/18 di"
	| selection |
	(self isAFileNamed: aFileName) ifTrue:
		[^ self new open: aFileName forWrite: true].

	"File does not exist..."
	selection _ (PopUpMenu labels: 'create a new file
choose another name
cancel')
			startUpWithCaption: (self localNameFor: aFileName) , '
does not exist.'.
	selection = 1 ifTrue:
		[^ self new open: aFileName forWrite: true].
	selection = 2 ifTrue:
		[^ self oldFileNamed:
			(FillInTheBlank request: 'Enter a new file name'
						initialAnswer: aFileName)].
	self halt! !

!StandardFileStream class methodsFor: 'file creation' stamp: 'di 10/4/97 10:21'!
readOnlyFileNamed: aFileName
	"Open a file of the given name for read-only access.  1/31/96 sw"
	| selection |
	(self isAFileNamed: aFileName) ifTrue:
		[^ self new open: aFileName forWrite: false].

	"File does not exist..."
	selection _ (PopUpMenu labels: 'choose another name
cancel')
			startUpWithCaption: (self localNameFor: aFileName) , '
does not exist.'.
	selection = 1 ifTrue:
		[^ self readOnlyFileNamed:
			(FillInTheBlank request: 'Enter a new file name'
						initialAnswer: aFileName)].
	self halt! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'di 10/3/97 11:13'!
removeAllUnSentMessages   "Smalltalk removeAllUnSentMessages" 
	"Remove all implementations of unsent messages."
	| sels n |
	sels _ self allUnSentMessages.

	"The following should be preserved for doIts, etc"
	#(dragon: hilberts: mandala: web test3 factorial benchmark benchFib
		newDepth: restoreAfter: forgetDoIts
		removeAllUnSentMessages abandonSources removeUnreferencedKeys
		reclaimDependents zapOrganization condenseChanges browseObsoleteReferences
		methodsFor:stamp: methodsFor:stamp:prior: instanceVariableNames:
		startTimerInterruptWatcher) do:
		[:sel | sels remove: sel ifAbsent: []].
	"The following may be sent by perform: in dispatchOnChar..."
	(ParagraphEditor classPool at: #CmdActions) asSet do:
		[:sel | sels remove: sel ifAbsent: []].
	(ParagraphEditor classPool at: #ShiftCmdActions) asSet do:
		[:sel | sels remove: sel ifAbsent: []].
	sels size = 0 ifTrue: [^ 0].

	n _ 0. Smalltalk allBehaviorsDo: [:x | n _ n+1].
	'Removing ', sels size printString , ' messages . . .'
		displayProgressAt: Sensor cursorPoint
		from: 0 to: n
		during:
		[:bar |
		n _ 0.
		self allBehaviorsDo:
			[:class | bar value: (n _ n+1).
			sels do:
				[:sel | class removeSelectorSimply: sel]]].
	MethodDictionary allInstancesDo: [:d | d rehash].
	^ sels size! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'di 10/4/97 09:11'!
removeEmptyMessageCategories
	"Smalltalk removeEmptyMessageCategories"
	Smalltalk garbageCollect.
	ClassOrganizer allInstances , (Array with: SystemOrganization) do:
		[:org | org categories do: 
			[:cat | (org listAtCategoryNamed: cat) isEmpty
				ifTrue: [org removeCategory: cat]]]! !


!SystemTracer methodsFor: 'initialization' stamp: 'di 10/3/97 16:13'!
writeFileHeader 
	file position: 0.  "info in header page"
	self write4Bytes: ($A asciiValue *100) + 2.  "version number:  6500+2"
	self write4Bytes: imageHeaderSize.  "File offset (bytes) of start of data"
							"same as base address (byte) of first object"
	self write4Bytes: maxOop.  "Length of data segment in words"
	self write4Bytes: 0.		"what you have to add to an oop to get"
							"an offset in the data portion of this file"
	self write4Bytes: (self mapAt: specialObjects).
	self write4Bytes: (hashGenerator next * 16rFFF asFloat) asInteger.  "next hash"
	self write4Bytes: Display width * 16r10000 + Display height.  "display size"
	file position > imageHeaderSize ifTrue: [self error: 'Header ran over allotted length'].
	file padTo: imageHeaderSize put: 0.  "Pad header page"
	file close! !

!SystemTracer methodsFor: 'initialization' stamp: 'di 10/3/97 16:13'!
writeImage: roots 
	imageHeaderSize _ 64.	"16 longs"
	file position: imageHeaderSize.  "Skip header section"
	maxOop _ 0.  "Starting oop"
	self initCompactClasses.
	specialObjects _ Smalltalk specialObjectsArray copy.
	specialObjects at: 29 put: compactClasses.
	"New oop of nil is needed before we find out from the trace."
	NewNil _ maxOop + ((self headersFor: nil withHash: 0) size-1*4).
	self trace: nil.  "In fact, this traverses the system by the time it's done!!"
	self trace: specialObjects.
	roots do: [:root | self trace: root].
	self writeFileHeader.
	^ Array with: maxOop! !


!TextCollectorController methodsFor: 'private' stamp: 'di 9/28/97 15:08'!
visibleAreas
	"Transcript dependents last controller visibleAreas"
	| visibleAreas rect remnants myTopController |
	myTopController _ self view topView controller.
	visibleAreas _ Array with: view insetDisplayBox.
	myTopController view uncacheBits.
	ScheduledControllers scheduledWindowControllers do:
		[:c | c == myTopController ifTrue: [^ visibleAreas].
		rect _ c view windowBox.
		remnants _ OrderedCollection new.
		visibleAreas do: [:a | remnants addAll: (a areasOutside: rect)].
		visibleAreas _ remnants].
	^ visibleAreas! !


SocketWithHeader removeFromSystem.
MorphicModel removeUninstantiatedModels.
SystemOrganization classify: #SystemMonitor under: 'System-Support'.
SystemOrganization classify: #MultiuserTinyPaint under: 'Morphic-Demo'.
Smalltalk removeEmptyMessageCategories.!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'di 10/4/97 10:58'!
version
	"Answer the version of this release."

	^ 'Squeak 1.23 of October 4, 1997'! !
